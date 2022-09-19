module FreeMethodist.BibleQuizTracker.Server.Tests.FailAppeal_UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.FailAppeal.Pipeline
open Xunit

[<Fact>]
let ``Given Question has not been appealed When appeal fails Then record failure with appealer's name`` () =
    result {
        let quizzer = "Jim"

        let setupCurrentQuizzer quiz =
            { quiz with CurrentQuizzer = (Some quizzer) }
            |> Arrange.withParticipants [ QuizzerState.create quizzer ]

        let initialQuiz =
            RunningQuiz.newTeamQuiz |> setupCurrentQuizzer

        let! result, _ = updateQuiz (quizzer, TeamOne) initialQuiz

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
                CurrentQuizzer = (Some quizzer) }
            |> Arrange.withParticipants [ QuizzerState.create quizzer ]
        let initialQuiz =
            RunningQuiz.newTeamQuiz |> setupCurrentQuizzer

        let! result, _ = updateQuiz (quizzer, TeamOne) initialQuiz

        let expectedAppeal =
            initialQuiz.TeamOne.Score |> QuizScore.failAppeal

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
                CurrentQuizzer = (Some quizzer) }
            |> Arrange.withParticipants [ QuizzerState.create quizzer ]
            |> Arrange.withTeamTwoParticipants [ QuizzerState.create previousAppealer ]

        let initialQuiz =
            RunningQuiz.newTeamQuiz
            |> setupCurrentQuizzer
            |> insertQuestion

        let! result, _ = updateQuiz (quizzer, TeamOne) initialQuiz

        let expectedAppeal =
            initialQuiz.TeamTwo.Score
            |> QuizScore.revertAppealFailure

        Assert.Equal(expectedAppeal, result.TeamTwo.Score)
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
                     |> fun q -> { q with FailedAppeal = Some quizzer }) }

    let setupCurrentQuizzer quiz =
        { quiz with
            CurrentQuizzer = (Some quizzer) }
        |> Arrange.withParticipants  [ QuizzerState.create quizzer ]

    let initialQuiz =
        RunningQuiz.newTeamQuiz
        |> setupCurrentQuizzer
        |> insertQuestion

    let result =
        updateQuiz (quizzer, TeamOne) initialQuiz

    Assert.Equal((Result.Error(FailAppeal.Error.AppealAlreadyFailed quizzer)), result)
