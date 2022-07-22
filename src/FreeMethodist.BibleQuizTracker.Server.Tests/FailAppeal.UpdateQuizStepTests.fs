module FreeMethodist.BibleQuizTracker.Server.Tests.FailAppeal_UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server.FailAppeal.Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.FailAppeal.Pipeline
open Xunit

[<Fact>]
let ``Given Question has not been appealed When appeal fails Then record failure with appealer's name`` () =
    result {
        let quizzer = "Jim"

        let setupCurrentQuizzer quiz =
            { quiz with
                CurrentQuizzer = (Some quizzer)
                TeamOne = { quiz.TeamOne with Quizzers = [ QuizzerState.create quizzer ] } }

        let initialQuiz =
            RunningTeamQuiz.identity |> setupCurrentQuizzer

        let! result = updateQuiz (quizzer,TeamOne) initialQuiz
        
        let expectedAppeal = Some quizzer
        Assert.Equal(expectedAppeal, result.Questions[result.CurrentQuestion].FailedAppeal)
    }
