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

    let result = updateQuiz quizzer quiz

    let error =
        Error(AnswerCorrectly.Error.QuizzerNotFound quizzer)

    Assert.Equal(error, result)

[<Fact>]
let ``When Quizzer Answers then individual score goes up`` () =
    let quizzer = QuizzerState.create "Jim"

    let initialQuiz =
        quizWithQuizzerOnTeamOne quizzer

    let result =
        updateQuiz quizzer.Name initialQuiz

    let expectedScore =
        TeamScore.initial |> TeamScore.correctAnswer

    assertSuccess result (fun (updatedQuiz) ->
        let updatedQuizzer =
            updatedQuiz.QuizState.TeamOne.Quizzers
            |> List.find (fun q -> q.Name = quizzer.Name)

        Assert.Equal(expectedScore, updatedQuizzer.Score))

[<Fact>]
let ``When Quizzer Answers then team score goes up`` () =
    let quizzer = QuizzerState.create "Jim"

    let initialQuiz =
        quizWithQuizzerOnTeamOne quizzer

    let result =
        updateQuiz quizzer.Name initialQuiz

    let expectedScore = TeamScore.ofQuestions 1
    assertSuccess result (fun (updatedQuiz) -> Assert.Equal(expectedScore, updatedQuiz.QuizState.TeamOne.Score))


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
        updateQuiz answerer.Name quiz

    assertSuccess result (fun (updatedQuiz) ->
        let updatedQuizzer =
            updatedQuiz.QuizState.TeamOne.Quizzers
            |> List.find (fun q -> q.Name = nonAnswerer.Name)

        Assert.Equal(nonAnswerer.Score, updatedQuizzer.Score))

[<Fact>]
let ``When Quizzer Answers then increment the current question`` () =
    let quizzer = QuizzerState.create "Jim"

    let initialQuiz =
        { quizWithQuizzerOnTeamOne quizzer with CurrentQuestion = PositiveNumber.one }

    let result =
        updateQuiz quizzer.Name initialQuiz

    assertSuccess result (fun (updatedQuiz) ->
        Assert.Equal(
            initialQuiz.CurrentQuestion
            |> PositiveNumber.increment,
            updatedQuiz.QuizState.CurrentQuestion
        ))

[<Fact>]
let ``When Quizzer Answers then record answered question for history`` () =
    let quizzer = QuizzerState.create "Jim"

    let initialQuiz =
        { (quizWithQuizzerOnTeamOne quizzer) with CurrentQuestion = PositiveNumber.one }

    let result =
        updateQuiz quizzer.Name initialQuiz

    let expectedQuestion =
        { Answerer = quizzer.Name
          IncorrectAnswerers = [] }
        |> CompletedQuestion.Answered
        |> QuizQuestion.Complete

    assertSuccess result (fun (updatedQuiz) ->
        Assert.Equal(Some expectedQuestion, updatedQuiz.QuizState.Questions.TryFind initialQuiz.CurrentQuestion))

[<Fact>]
let ``Given Quizzer already answered correctly When Quizzer Answers then Error`` () =
    let quizzer = QuizzerState.create "Jim"

    let alreadyAnsweredQuestion =
        { Answerer = quizzer.Name
          IncorrectAnswerers = [] }
        |> Answered
        |> Complete

    let quizWithQuizzerOnTeamOne =
        (quizWithQuizzerOnTeamOne quizzer)

    let initialQuiz =
        { quizWithQuizzerOnTeamOne with
            CurrentQuestion = PositiveNumber.one
            Questions =
                quizWithQuizzerOnTeamOne.Questions
                |> Map.add PositiveNumber.one alreadyAnsweredQuestion }

    let result =
        updateQuiz quizzer.Name initialQuiz

    Assert.Equal(Result.Error (AnswerCorrectly.QuizzerAlreadyAnsweredCorrectly ( QuizQuestion.QuizzerAlreadyAnsweredCorrectly (quizzer.Name, initialQuiz.CurrentQuestion))), result)
