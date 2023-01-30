module FreeMethodist.BibleQuizTracker.Server.Persistence

open System
open System.Collections.Generic
open System.Text.Json
open System.Threading
open Azure
open Azure.Storage.Blobs
open Azure.Storage.Blobs.Models
open FreeMethodist.BibleQuizTracker.Server.Tournament
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open Microsoft.AspNetCore.Components.Server.ProtectedBrowserStorage
open Microsoft.FSharp.Control


let initExample quizCode =
    let teamOne: QuizTeamState =
        { Name = "LEFT"
          Score = QuizScore.ofQuestions 1
          Quizzers =
            [ { Name = "Jim"
                Score = QuizScore.ofQuestions 1
                Participation = In }
              { Name = "John"
                Score = QuizScore.zero
                Participation = In } ] }

    let teamTwo: QuizTeamState =
        { Name = "RIGHT"
          Score = QuizScore.ofQuestions 2
          Quizzers =
            [ { Name = "Jina"
                Score = QuizScore.ofQuestions 2
                Participation = In }
              { Name = "Juni"
                Score = QuizScore.zero
                Participation = In } ] }

    Running
        { Code = quizCode
          CompetitionStyle = RunningCompetitionStyle.Team(teamOne, teamTwo)
          CurrentQuestion = (PositiveNumber.one |> PositiveNumber.increment |> PositiveNumber.increment)
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
                (PositiveNumber.one |> PositiveNumber.increment |> PositiveNumber.increment)
                ({ Answerer = "Jina"
                   IncorrectAnswerers = [] }
                 |> Answered
                 |> Complete)
            |> Map.map (fun key value -> { QuestionState.initial with AnswerState = value })
          TournamentInfo = TournamentInfo.empty }



let getQuizFromLocalStorage (localStorage: ProtectedLocalStorage) deserialize : GetQuiz =
    fun quizCode ->
        asyncResult {
            let! quizJsonString =
                (localStorage.GetAsync<string>($"QUIZ-{quizCode}").AsTask() |> Async.AwaitTask)
                |> Async.map (fun json -> json.Value)
                |> AsyncResult.ofAsync

            return
                (if quizJsonString = null then None else Some quizJsonString)
                |> Option.map deserialize
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

            let json = JsonSerializer.Serialize(quiz, options)

            return!
                localStorage.SetAsync($"QUIZ-{code}", json).AsTask()
                |> Async.AwaitTask
                |> AsyncResult.ofAsync
        }


let validateBlobOperation (uploadResult: Response<'a>) =
    if uploadResult.GetRawResponse().IsError then
        (uploadResult.GetRawResponse().ReasonPhrase |> RemoteError |> Error)
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

let quizBlobName quizCode = $"quizzes/{getBlobName quizCode}"

let getQuizFromBlob (blobServiceClient: BlobServiceClient) deserialize (tenantName: string) : GetQuiz =
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

            let blobClient = quizCode |> quizBlobName |> blobContainerClient.GetBlobClient

            let! response =
                blobClient.DownloadContentAsync()
                |> Async.AwaitTask
                |> Async.Catch
                |> Async.map mapChoiceToResult

            let! response = validateBlobOperation response |> AsyncResult.ofResult

            let quizJson = response.Content.ToString()

            let quiz = deserialize quizJson

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
            let blobContainerClient =
                blobServiceClient.GetBlobContainerClient(tenantName.ToLower())

            do!
                blobContainerClient.CreateIfNotExistsAsync()
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.ignore

            let quizCode = quiz |> getCodeFromQuiz

            let! json =
                try
                    (JsonSerializer.Serialize(quiz, options) |> AsyncResult.ofSuccess)
                with exn ->
                    exn |> DbError.Exception |> AsyncResult.ofError

            let blobClient = quizCode |> quizBlobName |> blobContainerClient.GetBlobClient

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
        }

let private mapTournamentInfo quiz =
    match quiz with
    | Running runningTeamQuiz -> runningTeamQuiz.TournamentInfo
    | Completed completedTeamQuiz -> completedTeamQuiz.TournamentInfo
    | Official officialTeamQuiz -> officialTeamQuiz.TournamentInfo

let mapStyle quiz =
    match quiz with
    | Running runningTeamQuiz ->
        match runningTeamQuiz.CompetitionStyle with
        | RunningCompetitionStyle.Individuals _ -> "Individual"
        | RunningCompetitionStyle.Team _ -> "Team"
    | Completed completedQuiz ->
        match completedQuiz.CompetitionStyle with
        | CompletedCompetitionStyle.Individual _ -> "Individual"
        | CompletedCompetitionStyle.Team _ -> "Team"
    | Official officialQuiz ->
        match officialQuiz.CompetitionStyle with
        | OfficialCompetitionStyle.Individual _ -> "Individual"
        | OfficialCompetitionStyle.Team _ -> "Team"


let saveNewQuizToBlob
    (blobServiceClient: BlobServiceClient)
    (options: JsonSerializerOptions)
    (tenantName: string)
    : SaveNewQuiz =
    fun quiz ->
        asyncResult {
            let blobContainerClient =
                blobServiceClient.GetBlobContainerClient(tenantName.ToLower())

            do!
                blobContainerClient.CreateIfNotExistsAsync()
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.ignore

            let quizCode = quiz |> getCodeFromQuiz

            let! json =
                try
                    (JsonSerializer.Serialize(quiz, options) |> AsyncResult.ofSuccess)
                with exn ->
                    exn |> DbError.Exception |> AsyncResult.ofError

            let blobClient = quizCode |> quizBlobName |> blobContainerClient.GetBlobClient

            let tournamentInfo = quiz |> mapTournamentInfo

            let tournamentTags =
                [ tournamentInfo.Link
                  |> function
                      | None -> None
                      | Some (Id _) -> None
                      | Some (Name name) -> Some name
                  |> Option.map (fun tournament -> "Tournament", tournament)
                  tournamentInfo.Church |> Option.map (fun church -> ("Church", church))
                  tournamentInfo.Room |> Option.map (fun room -> ("Room", room))
                  tournamentInfo.Round |> Option.map (fun round -> ("Round", round))
                  tournamentInfo.GradeDivision
                  |> Option.map (fun division ->
                      let division =
                          match division with
                          | GradeDivision.Kids -> "Kids"
                          | GradeDivision.SeniorTeen -> "Senior Teen"
                          | GradeDivision.YoungTeen -> "Young Teen"
                          | GradeDivision.QUIC -> "QUIC"
                          | GradeDivision.Custom name -> name

                      ("GradeDivision", division))
                  tournamentInfo.CompetitionDivision
                  |> Option.map (fun division ->
                      let division =
                          match division with
                          | CompetitionDivision.Veteran -> "Veteran"
                          | CompetitionDivision.Rookie -> "Rookie"

                      ("CompetitionDivision", division)) ]
                |> List.choose id

            let tags =
                ([ "Code", quizCode; "State", mapState quiz; "CompetitionStyle", mapStyle quiz ]
                 @ tournamentTags
                 |> Map.ofList)

            let uploadOptions =
                BlobUploadOptions(
                    Conditions = BlobRequestConditions(IfNoneMatch = ETag("*")),

                    Tags = tags

                )

            do!
                json
                |> BinaryData.FromString
                |> fun data ->
                    blobClient.UploadAsync(data, uploadOptions, CancellationToken.None)
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

let tryGetQuizFromBlob (blobServiceClient: BlobServiceClient) deserialize (tenantName: string) : TryGetQuiz =

    fun quizCode ->
        asyncResult {

            let blobContainerClient =
                blobServiceClient.GetBlobContainerClient(tenantName.ToLower())

            let blobClient = quizCode |> quizBlobName |> blobContainerClient.GetBlobClient

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

let tryGetQuizFromLocalStorage (localStorage: ProtectedLocalStorage) deserialize : TryGetQuiz =
    fun quizCode ->
        asyncResult {
            let! quizJsonString =
                (localStorage.GetAsync<string>($"QUIZ-{quizCode}").AsTask() |> Async.AwaitTask)
                |> Async.map (fun storageResult -> storageResult.Value)
                |> AsyncResult.ofAsync

            return
                (if quizJsonString = null then None else Some quizJsonString)
                |> Option.map deserialize
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
