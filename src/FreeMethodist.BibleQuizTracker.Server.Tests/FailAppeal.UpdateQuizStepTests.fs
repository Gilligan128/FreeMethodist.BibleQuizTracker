module FreeMethodist.BibleQuizTracker.Server.Tests.FailAppeal_UpdateQuizStepTests

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
    |> Assert.onSuccess (fun (quiz) ->

        Assert.Equal<Quizzer list>([ quizzer ], quiz.Questions[quiz.CurrentQuestion].FailedAppeals))

[<Fact>]
let ``When appeal fails Then change Team score`` () =
    result {
        let quizzer = "Jim"

        let setupCurrentQuizzer quiz =
            { quiz with CurrentQuizzer = (Some quizzer) }
            |> Arrange.withParticipants [ QuizzerState.create quizzer ]

        let initialQuiz = RunningQuiz.newTeamQuiz |> setupCurrentQuizzer

        let! result = updateQuiz (quizzer, Some TeamOne) initialQuiz

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
let ``Given Quiz is Individuals When appeal fails Then change Individual score`` () =
    result {
        let quizzer = "Jim"

        let initialQuiz =
            RunningQuiz.newIndividualQuiz
            |> fun quiz -> { quiz with CurrentQuizzer = (Some quizzer) }
            |> Arrange.withParticipants [ QuizzerState.create quizzer ]

        let! result = updateQuiz (quizzer, None) initialQuiz

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
