module FreeMethodist.BibleQuizTracker.Server.Persistence

open System
open System.Text.Json
open System.Threading
open Azure
open Azure.Storage.Blobs
open Azure.Storage.Blobs.Models
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open Microsoft.AspNetCore.Components.Server.ProtectedBrowserStorage
open Microsoft.FSharp.Control


let initExample quizCode =
    Running
        { Code = quizCode
          TeamOne =
            { Name = "LEFT"
              Score = TeamScore.ofQuestions 1
              Quizzers =
                [ { Name = "Jim"
                    Score = TeamScore.ofQuestions 1
                    Participation = In }
                  { Name = "John"
                    Score = TeamScore.initial
                    Participation = In } ] }
          TeamTwo =
            { Name = "RIGHT"
              Score = TeamScore.ofQuestions 2
              Quizzers =
                [ { Name = "Jina"
                    Score = TeamScore.ofQuestions 2
                    Participation = In }
                  { Name = "Juni"
                    Score = TeamScore.initial
                    Participation = In } ] }
          CurrentQuestion =
            (PositiveNumber.one
             |> PositiveNumber.increment
             |> PositiveNumber.increment)
          CurrentQuizzer = Some "Juni"
          Questions =
            Map.empty
            |> Map.add
                PositiveNumber.one
                ({ Answerer = "Jim"
                   IncorrectAnswerers = [] }
                 |> Answered
                 |> Complete)
            |> Map.add
                (PositiveNumber.one |> PositiveNumber.increment)
                ({ Answerer = "Jina"
                   IncorrectAnswerers = [] }
                 |> Answered
                 |> Complete)
            |> Map.add
                (PositiveNumber.one
                 |> PositiveNumber.increment
                 |> PositiveNumber.increment)
                ({ Answerer = "Jina"
                   IncorrectAnswerers = [] }
                 |> Answered
                 |> Complete)
            |> Map.map (fun key value -> { QuestionState.initial with AnswerState = value }) }



let getQuizFromLocalStorage (localStorage: ProtectedLocalStorage) (options: JsonSerializerOptions) : GetQuiz =
    fun quizCode ->
        asyncResult {
            let! quizJsonString =
                (localStorage
                    .GetAsync<string>($"QUIZ-{quizCode}")
                     .AsTask()
                 |> Async.AwaitTask)
                |> Async.map (fun json -> json.Value)
                |> AsyncResult.ofAsync

            return
                (if quizJsonString = null then
                     None
                 else
                     Some quizJsonString)
                |> Option.map (fun json -> JsonSerializer.Deserialize<Quiz>(json, options))
                |> fun quizOpt ->
                    match quizOpt with
                    | None -> initExample quizCode
                    | Some quiz -> quiz
        }

let getCodeFromQuiz quiz =
    match quiz with
    | Running runningTeamQuiz -> runningTeamQuiz.Code
    | Completed completedTeamQuiz -> completedTeamQuiz.Code
    | Official officialTeamQuiz -> officialTeamQuiz.Code

let getBlobName quizCode = $"quiz-{quizCode}"

let containerNameOld = "quizzes"

let saveQuizToLocalStorage (localStorage: ProtectedLocalStorage) (options: JsonSerializerOptions) : SaveQuiz =
    fun quiz ->
        asyncResult {
            let code = quiz |> getCodeFromQuiz

            let json =
                JsonSerializer.Serialize(quiz, options)

            return!
                localStorage
                    .SetAsync($"QUIZ-{code}", json)
                    .AsTask()
                |> Async.AwaitTask
                |> AsyncResult.ofAsync
        }


let validateBlobOperation (uploadResult: Response<'a>) =
    if uploadResult.GetRawResponse().IsError then
        (uploadResult.GetRawResponse().ReasonPhrase
         |> RemoteError
         |> Error)
    else
        Ok uploadResult.Value

let matchInnerException (exn: exn) =
    match exn.InnerException with
    | null -> None
    | _ -> Some(exn.InnerException)

let checkIfNotFound (exn: exn) =
    match exn.InnerException with
    | null -> false
    | :? RequestFailedException as ex when ex.Status = 404 -> true
    | _ -> false

let quizBlobName quizCode =
    $"quizzes/{getBlobName quizCode}"

let getQuizFromBlob (blobServiceClient: BlobServiceClient) (options: JsonSerializerOptions) (tenantName : string) : GetQuiz =
    let mapChoiceToResult choice =
        match choice with
        | Choice1Of2 one -> Ok one
        | Choice2Of2 exn ->
            exn
            |> matchInnerException
            |> Option.defaultValue exn
            |> Exception
            |> Result.Error

    fun quizCode ->
        asyncResult {

            let blobContainerClient =
                blobServiceClient.GetBlobContainerClient(tenantName.ToLower())

            let blobClient =
                quizCode
                |> quizBlobName
                |> blobContainerClient.GetBlobClient

            let! response =
                blobClient.DownloadContentAsync()
                |> Async.AwaitTask
                |> Async.Catch
                |> Async.map mapChoiceToResult

            let! response =
                validateBlobOperation response
                |> AsyncResult.ofResult

            let quizJson = response.Content.ToString()

            let quiz =
                JsonSerializer.Deserialize<Quiz>(quizJson, options)

            return quiz
        }

let private mapState quiz =
    match quiz with
    | Running _ -> nameof Running
    | Completed _ -> nameof Completed
    | Official _ -> nameof Official

let saveQuizToBlob
    (blobServiceClient: BlobServiceClient)
    (options: JsonSerializerOptions)
    (tenantName: string)
    : SaveQuiz =
    fun quiz ->
        asyncResult {
            let blobContainerClientOld =
                blobServiceClient.GetBlobContainerClient(containerNameOld)

            let blobContainerClient =
                blobServiceClient.GetBlobContainerClient(tenantName.ToLower())

            do!
                blobContainerClientOld.CreateIfNotExistsAsync()
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.ignore

            do!
                blobContainerClient.CreateIfNotExistsAsync()
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.ignore

            let quizCode = quiz |> getCodeFromQuiz

            let! json =
                try
                    (JsonSerializer.Serialize(quiz, options)
                     |> AsyncResult.ofSuccess)
                with
                | exn -> exn |> DbError.Exception |> AsyncResult.ofError

            let blobClientOld =
                quizCode
                |> getBlobName
                |> blobContainerClientOld.GetBlobClient

            let blobClient =
                quizCode
                |> quizBlobName
                |> blobContainerClient.GetBlobClient

            let uploadOptions =
                BlobUploadOptions(Tags = ([ "State", mapState quiz ] |> Map.ofList))

            let data = json |> BinaryData.FromString

            do!
                data
                |> fun data -> blobClient.UploadAsync(data, uploadOptions)
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.bind (fun response -> validateBlobOperation response |> AsyncResult.ofResult)
                |> AsyncResult.ignore

            do!
                data
                |> fun data -> blobClientOld.UploadAsync(data, uploadOptions)
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.bind (fun response -> validateBlobOperation response |> AsyncResult.ofResult)
                |> AsyncResult.ignore
        }


let saveNewQuizToBlob
    (blobServiceClient: BlobServiceClient)
    (options: JsonSerializerOptions)
    (tenantName: string)
    : SaveNewQuiz =
    fun quiz ->
        asyncResult {
            let blobContainerClientOld =
                blobServiceClient.GetBlobContainerClient(containerNameOld)

            let blobContainerClient =
                blobServiceClient.GetBlobContainerClient(tenantName.ToLower())

            do!
                blobContainerClientOld.CreateIfNotExistsAsync()
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.ignore

            do!
                blobContainerClient.CreateIfNotExistsAsync()
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.ignore


            let quizCode = quiz |> getCodeFromQuiz

            let! json =
                try
                    (JsonSerializer.Serialize(quiz, options)
                     |> AsyncResult.ofSuccess)
                with
                | exn -> exn |> DbError.Exception |> AsyncResult.ofError

            let blobClientOld =
                quizCode
                |> getBlobName
                |> blobContainerClientOld.GetBlobClient

            let blobClient =
                quizCode
                |> quizBlobName
                |> blobContainerClient.GetBlobClient

            let uploadOptions =
                BlobUploadOptions(
                    Conditions = BlobRequestConditions(IfNoneMatch = ETag("*")),
                    Tags =
                        ([ "Code", quizCode
                           "State", mapState quiz ]
                         |> Map.ofList)
                )

            do!
                json
                |> BinaryData.FromString
                |> fun data ->
                    blobClient.UploadAsync(data, uploadOptions, CancellationToken.None)
                    |> Async.AwaitTask
                    |> Async.map (fun _ -> data)
                |> Async.bind (fun data ->
                    blobClientOld.UploadAsync(data, uploadOptions, CancellationToken.None)
                    |> Async.AwaitTask)
                |> Async.map validateBlobOperation
                |> AsyncResult.ignore

        }
    |> SaveNewQuiz

let private isAnExampleQuiz (quizCode: QuizCode) = quizCode.ToUpper() = "EXAMPLE"

let getQuizFromLocalOrBlob getFromLocal getFromBlob : GetQuiz =
    fun quizCode ->
        if isAnExampleQuiz quizCode then
            getFromLocal quizCode
        else
            getFromBlob quizCode

let saveQuizToLocalOrBlob saveToLocal saveToBlob : SaveQuiz =
    fun quiz ->
        quiz
        |> getCodeFromQuiz
        |> isAnExampleQuiz
        |> function
            | true -> saveToLocal quiz
            | false -> saveToBlob quiz

let private mapChoiceToResult choice =
    match choice with
    | Choice1Of2 one -> Some one |> Ok
    | Choice2Of2 exn ->
        exn
        |> fun ex ->
            if checkIfNotFound ex then
                Ok None
            else
                ex
                |> matchInnerException
                |> Option.defaultValue ex
                |> DbError.Exception
                |> Error

let tryGetQuizFromBlob (blobServiceClient: BlobServiceClient) deserialize (tenantName : string) : TryGetQuiz =

    fun quizCode ->
        asyncResult {

            let blobContainerClient =
                blobServiceClient.GetBlobContainerClient(tenantName.ToLower())

            let blobClient =
                quizCode
                |> quizBlobName
                |> blobContainerClient.GetBlobClient

            let! response =
                blobClient.DownloadContentAsync()
                |> Async.AwaitTask
                |> Async.Catch
                |> Async.map mapChoiceToResult

            let! responseOpt =
                response
                |> Option.map validateBlobOperation
                |> Option.transpose
                |> AsyncResult.ofResult

            return
                responseOpt
                |> Option.map (fun response -> response.Content.ToString() |> deserialize)
        }

let tryGetQuizFromLocalStorage (localStorage: ProtectedLocalStorage) (options: JsonSerializerOptions) : TryGetQuiz =
    fun quizCode ->
        asyncResult {
            let! quizJsonString =
                (localStorage
                    .GetAsync<string>($"QUIZ-{quizCode}")
                     .AsTask()
                 |> Async.AwaitTask)
                |> Async.map (fun storageResult -> storageResult.Value)
                |> AsyncResult.ofAsync

            return
                (if quizJsonString = null then
                     None
                 else
                     Some quizJsonString)
                |> Option.map (fun json -> JsonSerializer.Deserialize<Quiz>(json, options))
                |> function
                    | None -> initExample quizCode |> Some
                    | Some quiz -> quiz |> Some
        }

let tryGetQuizFromLocalOrBlob getFromLocal getFromBlob : TryGetQuiz =
    fun quizCode ->
        if isAnExampleQuiz quizCode then
            getFromLocal quizCode
        else
            getFromBlob quizCode

let getRecentCompletedQuizzes (blobServiceClient: BlobServiceClient) =
    asyncResult {
        let blobContainerClient =
            blobServiceClient.GetBlobContainerClient(containerNameOld)

        let state = nameof Quiz.Completed
        let expression = $"\"State\" = '{state}'"

        if blobContainerClient.Exists().Value then

            let results =
                blobContainerClient.FindBlobsByTags(expression)
                |> Seq.map (fun item -> item.BlobName)
                |> Seq.toList

            return results
        else
            return []
    }
