module FreeMethodist.BibleQuizTracker.Server.Persistence

open System
open System.Text.Json
open Azure.Storage.Blobs
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open Microsoft.AspNetCore.Components.Server.ProtectedBrowserStorage


let initExample quizCode =
    Running
        { Code = quizCode
          TeamOne =
            { Name = "LEFT"
              Score = TeamScore.ofQuestions 1
              Captain = None
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
              Captain = None
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

let getQuizFromLocalStorage (localStorage: ProtectedLocalStorage) (options: JsonSerializerOptions) : GetTeamQuizAsync =
    fun quizCode ->
        async {
            let! quizJsonString =
                (localStorage
                    .GetAsync<string>($"QUIZ-{quizCode}")
                     .AsTask()
                 |> Async.AwaitTask)
                |> Async.map (fun json -> json.Value)

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

let getBlobName quizCode =
    $"quiz-{quizCode}"

let containerName = "quizzers"

let saveQuizToLocalStorage (localStorage: ProtectedLocalStorage) (options: JsonSerializerOptions) : SaveTeamQuizAsync =
    fun quiz ->
        async {
            let code = quiz |> getCodeFromQuiz

            let json =
                JsonSerializer.Serialize(quiz, options)

            return!
                localStorage
                    .SetAsync($"QUIZ-{code}", json)
                    .AsTask()
                |> Async.AwaitTask
        }

let getQuizFromBlob (blobServiceClient: BlobServiceClient) (options: JsonSerializerOptions) : GetTeamQuizAsync =
    fun quizCode ->
        async {
            let blobContainerClient = blobServiceClient.GetBlobContainerClient(containerName)
            
            let blobClient  = blobContainerClient.GetBlobClient($"quiz-{quizCode}")
            let! response = blobClient.DownloadContentAsync()|> Async.AwaitTask
            let quizJson = response.Value.Content.ToString()
            let quiz = JsonSerializer.Deserialize<Quiz>(quizJson, options)
            return quiz
        }
        
let saveQuizToBlob (blobServiceClient: BlobServiceClient) (options: JsonSerializerOptions) : SaveTeamQuizAsync =
    fun quiz ->
        async {
            let blobContainerClient = blobServiceClient.GetBlobContainerClient(containerName)
            let! containerResponse = blobContainerClient.CreateIfNotExistsAsync() |> Async.AwaitTask
            let quizCode =  quiz |> getCodeFromQuiz
            let json = JsonSerializer.Serialize(quiz, options)
            let blobClient = quizCode |> getBlobName |>  blobContainerClient.GetBlobClient
            do! json |> BinaryData.FromString |> blobClient.UploadAsync |> Async.AwaitTask |> Async.Ignore
            ()
        }