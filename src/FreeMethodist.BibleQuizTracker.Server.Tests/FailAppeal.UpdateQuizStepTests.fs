﻿module FreeMethodist.BibleQuizTracker.Server.Tests.FailAppeal_UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.FailAppeal.Pipeline
open Xunit

[<Fact>]
let ``Given Question has not been appealed When appeal fails Then record failure with appealer's name`` () =
    let quizzer = "Jim"

    let result =
        result {

            let setupCurrentQuizzer quiz =
                { quiz with CurrentQuizzer = (Some quizzer) }
                |> Arrange.withParticipants [ QuizzerState.create quizzer ]

            let initialQuiz = RunningQuiz.newTeamQuiz |> setupCurrentQuizzer

            return! updateQuiz (quizzer, Some TeamOne) initialQuiz
        }

    result
    |> Assert.onSuccess (fun (quiz, _) ->

        Assert.Equal<Quizzer list>([ quizzer ], quiz.Questions[quiz.CurrentQuestion].FailedAppeals))

[<Fact>]
let ``When appeal fails Then change Team score`` () =
    result {
        let quizzer = "Jim"

        let setupCurrentQuizzer quiz =
            { quiz with CurrentQuizzer = (Some quizzer) }
            |> Arrange.withParticipants [ QuizzerState.create quizzer ]

        let initialQuiz = RunningQuiz.newTeamQuiz |> setupCurrentQuizzer

        let! result, _ = updateQuiz (quizzer, Some TeamOne) initialQuiz

        let expectedAppeal =
            initialQuiz
            |> RunningQuiz.getTeam TeamPosition.TeamOne
            |> fun t -> t.Score |> QuizScore.failAppeal

        let teamOneScore =
            result
            |> RunningQuiz.getTeam TeamPosition.TeamOne
            |> fun t -> t.Score

        Assert.Equal(expectedAppeal, teamOneScore)
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
                         |> fun q -> { q with FailedAppeals = [ previousAppealer ] }) }

        let setupCurrentQuizzer quiz =
            { quiz with CurrentQuizzer = (Some quizzer) }
            |> Arrange.withParticipants [ QuizzerState.create quizzer ]
            |> Arrange.withTeamTwoParticipants [ QuizzerState.create previousAppealer ]

        let initialQuiz =
            RunningQuiz.newTeamQuiz
            |> setupCurrentQuizzer
            |> insertQuestion

        let! result, _ = updateQuiz (quizzer, Some TeamOne) initialQuiz

        let expectedAppeal =
            initialQuiz
            |> RunningQuiz.getTeamScore TeamPosition.TeamTwo
            |> QuizScore.revertAppealFailure

        Assert.Equal(
            expectedAppeal,
            result
            |> RunningQuiz.getTeamScore TeamPosition.TeamTwo
        )
    }

[<Fact>]
let ``Given the same quizzer preciously failed an appeal for this Question When appeal fails Then revert Error`` () =
    let quizzer = "Jim"

    let insertQuestion quiz =
        { quiz with
            Questions =
                quiz.Questions
                |> Map.add
                    quiz.CurrentQuestion
                    (QuestionState.initial
                     |> fun q -> { q with FailedAppeals = [ quizzer ] }) }

    let setupCurrentQuizzer quiz =
        { quiz with CurrentQuizzer = (Some quizzer) }
        |> Arrange.withParticipants [ QuizzerState.create quizzer ]

    let initialQuiz =
        RunningQuiz.newTeamQuiz
        |> setupCurrentQuizzer
        |> insertQuestion

    let result = updateQuiz (quizzer, Some TeamOne) initialQuiz

    Assert.Equal((Result.Error(FailAppeal.Error.AppealAlreadyFailed quizzer)), result)


[<Fact>]
let ``Given Quiz is Individuals When appeal fails Then change Individual score`` () =
    result {
        let quizzer = "Jim"

        let initialQuiz =
            RunningQuiz.newIndividualQuiz
            |> fun quiz -> { quiz with CurrentQuizzer = (Some quizzer) }
            |> Arrange.withParticipants [ QuizzerState.create quizzer ]

        let! result, _ = updateQuiz (quizzer, None) initialQuiz

        let expectedAppeal =
            initialQuiz
            |> RunningQuiz.findQuizzer quizzer
            |> fun (t, _) -> t.Score |> QuizScore.failAppeal

        let score =
            result
            |> RunningQuiz.findQuizzer quizzer
            |> fun (quizzerState, _) -> quizzerState.Score

        Assert.Equal(expectedAppeal, score)
    }
