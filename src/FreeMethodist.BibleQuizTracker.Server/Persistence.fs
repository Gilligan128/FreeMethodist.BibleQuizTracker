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
    | Completed completedTeamQuiz -> completedTeamQuiz.code
    | Official officialTeamQuiz -> officialTeamQuiz.Code
    | Unvalidated unvalidatedTeamQuiz -> unvalidatedTeamQuiz.Code

let getBlobName quizCode = $"quiz-{quizCode}"

let containerName = "quizzes"

let saveQuizToLocalStorage (localStorage: ProtectedLocalStorage) (options: JsonSerializerOptions) : SaveQuiz =
    fun quiz ->
        asyncResult {
            let code = quiz |> getCodeFromQuiz

            let json = JsonSerializer.Serialize(quiz, options)

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

let getQuizFromBlob (blobServiceClient: BlobServiceClient) (options: JsonSerializerOptions) : GetQuiz =
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

            let blobContainerClient = blobServiceClient.GetBlobContainerClient(containerName)

            let blobClient = blobContainerClient.GetBlobClient($"quiz-{quizCode}")

            let! response =
                blobClient.DownloadContentAsync()
                |> Async.AwaitTask
                |> Async.Catch
                |> Async.map mapChoiceToResult

            let! response =
                validateBlobOperation response
                |> AsyncResult.ofResult

            let quizJson = response.Content.ToString()

            let quiz = JsonSerializer.Deserialize<Quiz>(quizJson, options)

            return quiz
        }

let saveQuizToBlob (blobServiceClient: BlobServiceClient) (options: JsonSerializerOptions) : SaveQuiz =
    fun quiz ->
        asyncResult {
            let blobContainerClient = blobServiceClient.GetBlobContainerClient(containerName)

            do!
                blobContainerClient.CreateIfNotExistsAsync()
                |> Async.AwaitTask
                |> AsyncResult.ofAsync
                |> AsyncResult.ignore

            let quizCode = quiz |> getCodeFromQuiz

            let! json =
                try
                    (JsonSerializer.Serialize(quiz, options)
                     |> AsyncResult.ofSuccess)
                with
                | exn -> exn |> DbError.Exception |> AsyncResult.ofError

            let blobClient =
                quizCode
                |> getBlobName
                |> blobContainerClient.GetBlobClient

            do!
                json
                |> BinaryData.FromString
                |> fun data -> blobClient.UploadAsync(data, overwrite = true)
                |> Async.AwaitTask
                |> Async.map validateBlobOperation
                |> AsyncResult.ignore

        }

let saveNewQuizToBlob (blobServiceClient: BlobServiceClient) (options: JsonSerializerOptions) : SaveNewQuiz =
    fun quiz ->
        asyncResult {
            let blobContainerClient = blobServiceClient.GetBlobContainerClient(containerName)

            do!
                blobContainerClient.CreateIfNotExistsAsync()
                |> Async.AwaitTask
                |> AsyncResult.ofAsync
                |> AsyncResult.ignore

            let quizCode = quiz |> getCodeFromQuiz

            let! json =
                try
                    (JsonSerializer.Serialize(quiz, options)
                     |> AsyncResult.ofSuccess)
                with
                | exn -> exn |> DbError.Exception |> AsyncResult.ofError

            let blobClient =
                quizCode
                |> getBlobName
                |> blobContainerClient.GetBlobClient

            let uploadOptions =
                BlobUploadOptions(
                    Conditions = BlobRequestConditions(IfNoneMatch = ETag("*")),
                    Tags = ([ "Code", quizCode ] |> Map.ofList)
                )

            do!
                json
                |> BinaryData.FromString
                |> fun data -> blobClient.UploadAsync(data, uploadOptions, CancellationToken.None)
                |> Async.AwaitTask
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


let tryGetQuizFromBlob (blobServiceClient: BlobServiceClient) deserialize : TryGetQuiz =
    let mapChoiceToResult choice =
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

    fun quizCode ->
        asyncResult {

            let blobContainerClient = blobServiceClient.GetBlobContainerClient(containerName)

            let blobClient = blobContainerClient.GetBlobClient($"quiz-{quizCode}")

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
        }

let tryGetQuizFromLocalOrBlob getFromLocal getFromBlob : TryGetQuiz =
    fun quizCode ->
        if isAnExampleQuiz quizCode then
            getFromLocal quizCode
        else
            getFromBlob quizCode
