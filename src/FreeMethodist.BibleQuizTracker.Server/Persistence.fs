module FreeMethodist.BibleQuizTracker.Server.Persistence

open System.Text.Json
open System.Text.Json.Serialization
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Pipeline
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
          Questions = Map.empty }

let mutable private exampleQuiz: Quiz =
    initExample "Example"

let getQuizFromMemory: GetTeamQuizAsync =
    fun _ -> Async.retn exampleQuiz


let saveQuizToMemory: SaveTeamQuizAsync =
    fun quiz ->
        exampleQuiz <- quiz
        Async.retn ()

let getQuizFromLocalStorage (localStorage: ProtectedLocalStorage) (options: JsonSerializerOptions) : GetTeamQuizAsync =
    fun quizCode ->
        async {
            let! quizJsonString =
                (localStorage
                    .GetAsync<string>($"QUIZ-{quizCode}")
                     .AsTask()
                 |> Async.AwaitTask)

            return
                (if quizJsonString.Value = null then
                     None
                 else
                     Some quizJsonString.Value)
                |> Option.map (fun json -> JsonSerializer.Deserialize<Quiz>(json, options))
                |> fun quizOpt ->
                    match quizOpt with
                    | None -> initExample quizCode
                    | Some quiz -> quiz
        }

let saveQuizToLocalStorage (localStorage: ProtectedLocalStorage) (options: JsonSerializerOptions) : SaveTeamQuizAsync =
    fun quiz ->
        async {
            let code =
                match quiz with
                | Running runningTeamQuiz -> runningTeamQuiz.Code
                | Completed completedTeamQuiz -> completedTeamQuiz.code
                | Official officialTeamQuiz -> officialTeamQuiz.Code
                | Unvalidated unvalidatedTeamQuiz -> unvalidatedTeamQuiz.Code

            let json = JsonSerializer.Serialize(quiz, options)

            return! localStorage.SetAsync($"QUIZ-{code}", json).AsTask() |> Async.AwaitTask
        }
