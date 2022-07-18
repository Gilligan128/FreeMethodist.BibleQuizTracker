module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerCorrectlyWorkflowTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit

[<Fact>]
let ``Quizzer Answers who is not participating results in error`` () =
    let quiz =
        { RunningTeamQuiz.identity with
            TeamOne = { RunningTeamQuiz.identity.TeamOne with Quizzers = [ QuizzerState.create "Jim" ] }
            TeamTwo = { RunningTeamQuiz.identity.TeamTwo with Quizzers = [QuizzerState.create "Jessie"] } }

    let quizzer = "Not Participating"

    let result =
        AnswerCorrectly_Pipeline.updateQuiz quizzer quiz

    let error =
        Error(AnswerCorrectly.Error.QuizzerNotFound quizzer)

    Assert.Equal(error, result)

