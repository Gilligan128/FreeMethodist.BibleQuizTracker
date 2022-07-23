module FreeMethodist.BibleQuizTracker.Server.Tests.ClearAppeal.UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Workflow
open FreeMethodist.BibleQuizTracker.Server.FailAppeal.Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Pipeline
open Xunit

[<Fact>]
let ``Given Question has  been appealed When appeal clears Then record remove appeal with appealer's name`` () =
    result {
        let quizzer = "Jim"

        let setupCurrentQuizzer quiz =
            { quiz with
                CurrentQuizzer = (Some quizzer)
                TeamOne = { quiz.TeamOne with Quizzers = [ QuizzerState.create quizzer ] } }

        let insertAppeal quiz =
            { quiz with
                Questions =
                    quiz.Questions
                    |> Map.add
                        quiz.CurrentQuestion
                        (QuestionState.initial
                         |> QuestionState.failAppeal quizzer) }

        let initialQuiz =
            RunningTeamQuiz.identity
            |> setupCurrentQuizzer
            |> insertAppeal

        let! result, _ = updateQuiz initialQuiz

        Assert.Equal(
            None,
            result.Questions[result.CurrentQuestion]
                .FailedAppeal
        )
    }
[<Fact>]
let ``Given someone  preciously failed an appeal for this Question When appeal clears Then revert previous appealer's team score``
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

        let! result, _ = updateQuiz initialQuiz

        let expectedAppeal =
            initialQuiz.TeamTwo.Score
            |> TeamScore.revertAppealFailure

        Assert.Equal(expectedAppeal, result.TeamTwo.Score)
    }

[<Fact>]
let ``Given no one failed an appeal for this Question When appeal clears Then Error``
    ()
    =
    let quizzer = "Jim"

    let insertQuestion quiz =
        { quiz with
            Questions =
                quiz.Questions
                |> Map.add
                    quiz.CurrentQuestion
                    (QuestionState.initial
                     |> fun q -> { q with FailedAppeal = None }) }

    let setupCurrentQuizzer quiz =
        { quiz with
            CurrentQuizzer = (Some quizzer)
            TeamOne = { quiz.TeamOne with Quizzers = [ QuizzerState.create quizzer ] }}

    let initialQuiz =
        RunningTeamQuiz.identity
        |> setupCurrentQuizzer
        |> insertQuestion

    let result = updateQuiz initialQuiz

    Assert.Equal(Error ClearAppeal.Error.NoFailedAppeal, result)