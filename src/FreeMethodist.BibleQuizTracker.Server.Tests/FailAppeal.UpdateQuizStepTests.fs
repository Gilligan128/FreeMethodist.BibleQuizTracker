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

        let! result = updateQuiz (quizzer, TeamOne) initialQuiz

        let expectedAppeal = Some quizzer

        Assert.Equal(
            expectedAppeal,
            result.Questions[result.CurrentQuestion]
                .FailedAppeal
        )
    }

[<Fact>]
let ``When appeal fails Then change Team score`` () =
    result {
        let quizzer = "Jim"

        let setupCurrentQuizzer quiz =
            { quiz with
                CurrentQuizzer = (Some quizzer)
                TeamOne = { quiz.TeamOne with Quizzers = [ QuizzerState.create quizzer ] } }

        let initialQuiz =
            RunningTeamQuiz.identity |> setupCurrentQuizzer

        let! result = updateQuiz (quizzer, TeamOne) initialQuiz

        let expectedAppeal =
            initialQuiz.TeamOne.Score |> TeamScore.failAppeal

        Assert.Equal(expectedAppeal, result.TeamOne.Score)
    }

[<Fact>]
let ``Given someone else preciously failed an appeal for this Question When appeal fails Then revert previous appealer's team score``
    ()
    =
    result {
        let quizzer = "Jim"
        let previousAppealer = "Previous"

        let insertQuestion quiz =
            { quiz with
                Questions =
                    quiz.Questions
                    |> Map.add
                        quiz.CurrentQuestion
                        (QuestionState.initial
                         |> fun q -> { q with FailedAppeal = Some previousAppealer }) }

        let setupCurrentQuizzer quiz =
            { quiz with
                CurrentQuizzer = (Some quizzer)
                TeamOne = { quiz.TeamOne with Quizzers = [ QuizzerState.create quizzer ] }
                TeamTwo = { quiz.TeamTwo with Quizzers = [ QuizzerState.create previousAppealer ] } }

        let initialQuiz =
            RunningTeamQuiz.identity
            |> setupCurrentQuizzer
            |> insertQuestion

        let! result = updateQuiz (quizzer, TeamOne) initialQuiz

        let expectedAppeal =
            initialQuiz.TeamTwo.Score |> TeamScore.revertAppealFailure

        Assert.Equal(expectedAppeal, result.TeamTwo.Score)
    }
