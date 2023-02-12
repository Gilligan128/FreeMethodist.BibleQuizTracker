module FreeMethodist.BibleQuizTracker.Server.Tests.BackwardsCompatibilityTests

open System.Text.Json
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.QuizState_Versioning
open FreeMethodist.BibleQuizTracker.Server.Serialization
open Xunit

[<Fact>]
let ``Current quiz schema deserializes the same as it is serialized`` () =
   let fsharpOptions = FSharpSerializer.fSharpOptions
   do fsharpOptions.Converters.Add(QuizBackwardsCompatibilityConverter())
   
   let quiz = RunningQuiz.newTeamQuiz |> Quiz.Running
   let json = JsonSerializer.Serialize(quiz, fsharpOptions)
   let deserializedQuiz = JsonSerializer.Deserialize<Quiz>(json, fsharpOptions)
   Assert.Equal(quiz, deserializedQuiz)
    