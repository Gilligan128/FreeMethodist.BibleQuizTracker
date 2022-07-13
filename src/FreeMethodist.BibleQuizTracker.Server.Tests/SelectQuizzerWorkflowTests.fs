module FreeMethodist.BibleQuizTracker.Server.Tests.SelectQuizzerWorkflowTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit

[<Fact>]
let ``When Selecting Quizzer then that is now the quiz's current quizzer`` () =
    let quiz = RunningTeamQuiz.identity

    let result =
        SelectQuizzer_Pipeline.changeCurrentQuizzer "Jordan" quiz

    Assert.Equal("Jordan", result.CurrentQuizzer)
