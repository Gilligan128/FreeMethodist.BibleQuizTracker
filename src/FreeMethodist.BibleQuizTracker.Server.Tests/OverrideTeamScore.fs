module FreeMethodist.BibleQuizTracker.Server.Tests.RunQuiz.OverrideTeamScore

open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Api
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Pipeline
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain

open Xunit

//[<Fact>]
//let ``When Overriding Team Score`` () =
//    let quiz =
//            { TeamOne =
//                { Name = "One"
//                  Quizzers = []
//                  captain = Some "" }
//              TeamTwo =
//                { Name = "Two"
//                  Quizzers = []
//                  captain = Some "" }
//              code = ""
//              questions = [] }
//   
//    let scoreResult = (TeamScore.create 20) |> Result.map (fun x -> { Team = quiz.TeamOne.Name; NewScore = x })
//    let validationResult = scoreResult |> Result.bind (fun score -> validateTeamScore (fun _ ->  unbox quiz) "" score)
//    match validationResult with
//       | Ok score ->
//           Assert.Equal(score.value, 20)
//       | Error e -> Assert.True(true)
//    ignore
      
