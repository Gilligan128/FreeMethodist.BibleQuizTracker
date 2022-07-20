﻿module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerCorrectly.UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Tests.Common
open Xunit


let quizWithQuizzerOnTeamOne quizzer =
    { RunningTeamQuiz.identity with TeamOne = { RunningTeamQuiz.identity.TeamOne with Quizzers = [ quizzer ] } }

[<Fact>]
let ``Quizzer Answers who is not participating results in error`` () =
    let quiz =
        { RunningTeamQuiz.identity with
            TeamOne = { RunningTeamQuiz.identity.TeamOne with Quizzers = [ QuizzerState.create "Jim" ] }
            TeamTwo = { RunningTeamQuiz.identity.TeamTwo with Quizzers = [ QuizzerState.create "Jessie" ] } }

    let quizzer = "Not Participating"

    let result = updateQuiz (Some quizzer) quiz

    let error =
        Error(AnswerCorrectly.Error.QuizzerNotFound quizzer)

    Assert.Equal(error, result)

[<Fact>]
let ``When Quizzer Answers then individual score goes up`` () =
    let quizzer = QuizzerState.create "Jim"

    let initialQuiz =
        quizWithQuizzerOnTeamOne quizzer

    let result =
        updateQuiz (Some quizzer.Name) initialQuiz

    let expectedScore =
        TeamScore.initial |> TeamScore.correctAnswer

    assertSuccess result (fun (updatedQuiz, answerer) ->
        let updatedQuizzer =
            updatedQuiz.TeamOne.Quizzers
            |> List.find (fun q -> q.Name = answerer)

        Assert.Equal(expectedScore, updatedQuizzer.Score))

[<Fact>]
let ``When Quizzer Answers then team score goes up`` () =
    let quizzer = QuizzerState.create "Jim"

    let initialQuiz =
        quizWithQuizzerOnTeamOne quizzer

    let result =
        updateQuiz (Some quizzer.Name) initialQuiz

    let expectedScore = TeamScore.ofQuestions 1
    assertSuccess result (fun (updatedQuiz, _) -> Assert.Equal(expectedScore, updatedQuiz.TeamOne.Score))


[<Fact>]
let ``When Quizzer Answers then only updates score of answering quizzer`` () =
    let answerer =
        QuizzerState.create "Answerer"

    let nonAnswerer = QuizzerState.create "Jim"

    let quiz =
        { RunningTeamQuiz.identity with
            TeamOne = { RunningTeamQuiz.identity.TeamOne with Quizzers = [ nonAnswerer; answerer ] }
            TeamTwo = { RunningTeamQuiz.identity.TeamTwo with Quizzers = [ QuizzerState.create "Jessie" ] } }

    let result =
        updateQuiz (Some answerer.Name) quiz

    assertSuccess result (fun (updatedQuiz, _) ->
        let updatedQuizzer =
            updatedQuiz.TeamOne.Quizzers
            |> List.find (fun q -> q.Name = nonAnswerer.Name)

        Assert.Equal(nonAnswerer.Score, updatedQuizzer.Score))

[<Fact>]
let ``When Quizzer Answers then increment the current question`` () =
    let quizzer = QuizzerState.create "Jim"

    let initialQuiz =
        { quizWithQuizzerOnTeamOne quizzer with CurrentQuestion = PositiveNumber.one }

    let result =
        updateQuiz (Some quizzer.Name) initialQuiz

    assertSuccess result (fun (updatedQuiz, quizzer) ->
        Assert.Equal(
            initialQuiz.CurrentQuestion
            |> PositiveNumber.increment,
            updatedQuiz.CurrentQuestion
        ))

[<Fact>]
let ``When Quizzer Answers then record answered question for history`` () =
    let quizzer = QuizzerState.create "Jim"

    let initialQuiz =
        { (quizWithQuizzerOnTeamOne quizzer) with CurrentQuestion = PositiveNumber.one }

    let result =
        updateQuiz (Some quizzer.Name) initialQuiz

    let expectedQuestion =
        { Answerer = quizzer.Name
          IncorrectAnswerers = [] }
        |> CompletedQuestion.Answered
        |> QuizQuestion.Complete

    assertSuccess result (fun (updatedQuiz, _) ->
        Assert.Equal(Some expectedQuestion, updatedQuiz.Questions.TryFind updatedQuiz.CurrentQuestion))
