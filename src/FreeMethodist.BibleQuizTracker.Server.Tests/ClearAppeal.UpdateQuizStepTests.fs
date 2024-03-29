﻿module FreeMethodist.BibleQuizTracker.Server.Tests.ClearAppeal.UpdateQuizStepTests

open System
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Pipeline
open Xunit

[<Fact>]
let ``Given Question has  been appealed When appeal clears Then record remove appeal with appealer's name`` () =
    result {
        let quizzer = "Jim"

        let setupCurrentQuizzer quiz =
            { quiz with CurrentQuizzer = (Some quizzer) }
            |> Arrange.withParticipants [ QuizzerState.create quizzer ]

        let insertAppeal quiz =
            { quiz with
                Questions =
                    quiz.Questions
                    |> Map.add
                        quiz.CurrentQuestion
                        (QuestionState.initial
                         |> QuestionState.failAppeal quizzer) }

        let initialQuiz =
            RunningQuiz.newTeamQuiz
            |> setupCurrentQuizzer
            |> insertAppeal

        let! result, _ = updateQuiz initialQuiz

        Assert.Empty(
            result.Questions[result.CurrentQuestion]
                .FailedAppeals
        )
    }

let getTeamScore teamPosition quiz =
    quiz
    |> RunningQuiz.getTeam teamPosition
    |> fun t -> t.Score


[<Fact>]
let ``Given someone  previously failed an appeal for this Question When appeal clears Then revert previous appealer's team score``
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
                         |> fun q -> { q with FailedAppeals = [previousAppealer] }) }

        let setupCurrentQuizzer quiz =
            { quiz with CurrentQuizzer = (Some quizzer) }
            |> Arrange.withParticipants [ QuizzerState.create quizzer ]
            |> Arrange.withTeamTwoParticipants [ QuizzerState.create previousAppealer ]

        let initialQuiz =
            RunningQuiz.newTeamQuiz
            |> setupCurrentQuizzer
            |> insertQuestion

        let! result, _ = updateQuiz initialQuiz


        let expectedAppeal =
            initialQuiz
            |> getTeamScore TeamPosition.TeamTwo
            |> QuizScore.revertAppealFailure

        Assert.Equal(expectedAppeal, result |> getTeamScore TeamPosition.TeamTwo)
    }
    
    
[<Fact>]
let ``Given two quizzers  previously failed an appeal for this Question When appeal clears Then revert both appealer's team scores``
    ()
    =
    result {
        let quizzer = "Jim"
        let previousAppealer1 = "Previous1"
        let previousAppealer2 = "Previous2"

        let insertQuestion quiz =
            { quiz with
                Questions =
                    quiz.Questions
                    |> Map.add
                        quiz.CurrentQuestion
                        (QuestionState.initial
                         |> fun q -> { q with FailedAppeals = [previousAppealer1; previousAppealer2] }) }

        let setupCurrentQuizzer quiz =
            { quiz with CurrentQuizzer = (Some quizzer) }
            |> Arrange.withParticipants [ QuizzerState.create quizzer ]
            |> Arrange.withTeamTwoParticipants ([ previousAppealer1; previousAppealer2 ] |> List.map QuizzerState.create) 

        let initialQuiz =
            RunningQuiz.newTeamQuiz
            |> setupCurrentQuizzer
            |> insertQuestion

        let! result, _ = updateQuiz initialQuiz


        let expectedAppeal =
            initialQuiz
            |> getTeamScore TeamPosition.TeamTwo
            |> QuizScore.revertAppealFailure
            |> QuizScore.revertAppealFailure 

        Assert.Equal(expectedAppeal, result |> getTeamScore TeamPosition.TeamTwo)
    }
    
    
[<Fact>]
let ``Given no one failed an appeal for this Question When appeal clears Then Error`` () =
    let quizzer = "Jim"

    let insertQuestion quiz =
        { quiz with
            Questions =
                quiz.Questions
                |> Map.add
                    quiz.CurrentQuestion
                    (QuestionState.initial
                     |> fun q -> { q with  FailedAppeals = [] }) }

    let setupCurrentQuizzer quiz =
        { quiz with CurrentQuizzer = (Some quizzer) }
        |> Arrange.withParticipants [ QuizzerState.create quizzer ]

    let initialQuiz =
        RunningQuiz.newTeamQuiz
        |> setupCurrentQuizzer
        |> insertQuestion

    let result = updateQuiz initialQuiz

    Assert.Equal(Error ClearAppeal.Error.NoFailedAppeal, result)

[<Fact>]
let ``Given Quiz is Individuals When appeal cleared Then change Individual score`` () =
    let quizzer = "Jim"

    let initialQuiz =
        RunningQuiz.newIndividualQuiz
        |> fun quiz -> { quiz with CurrentQuizzer = (Some quizzer) }
        |> Arrange.withParticipants [ QuizzerState.create quizzer ]
        |> fun quiz ->
            { quiz with
                Questions =
                    quiz.Questions
                    |> Map.add
                        quiz.CurrentQuestion
                        (QuestionState.initial
                         |> fun q -> { q with FailedAppeals = [quizzer] }) }

    let expectedAppeal =
        initialQuiz
        |> RunningQuiz.findQuizzer quizzer
        |> fun (t, _) -> t.Score |> QuizScore.revertAppealFailure

    let result =
        result {

            let! result, _ = updateQuiz initialQuiz

            return
                result
                |> RunningQuiz.findQuizzer quizzer
                |> fun (quizzerState, _) -> quizzerState.Score
        }

    result
    |> Assert.onSuccess (fun score -> Assert.Equal(expectedAppeal, score))
