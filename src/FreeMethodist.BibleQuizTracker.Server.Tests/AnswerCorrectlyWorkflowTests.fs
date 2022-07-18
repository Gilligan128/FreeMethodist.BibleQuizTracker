module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerCorrectlyWorkflowTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit


let assertSuccess result assertion =
    match result with
    | Error errorValue -> Assert.True(false, $"Received {errorValue}")
    | Ok success -> assertion success

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

    assertSuccess result (fun updatedQuiz ->
        let updatedQuizzer =
            updatedQuiz.TeamOne.Quizzers
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
    assertSuccess result (fun updatedQuiz -> Assert.Equal(expectedScore, updatedQuiz.TeamOne.Score))


[<Fact>]
let ``When Quizzer Answers then only updates score of answering quizzer`` () =
    let answerer = QuizzerState.create "Answerer"
    let nonAnswerer = QuizzerState.create "Jim"
    let quiz =
        { RunningTeamQuiz.identity with
            TeamOne = { RunningTeamQuiz.identity.TeamOne with Quizzers = [ nonAnswerer; answerer] }
            TeamTwo = { RunningTeamQuiz.identity.TeamTwo with Quizzers = [ QuizzerState.create "Jessie" ] } }

    let result = updateQuiz answerer.Name quiz
  
    assertSuccess result (fun updatedQuiz ->
        let updatedQuizzer =
            updatedQuiz.TeamOne.Quizzers
            |> List.find (fun q -> q.Name = nonAnswerer.Name)

        Assert.Equal(nonAnswerer.Score, updatedQuizzer.Score))
 
[<Fact>]
let ``When Quizzer Answers then change current question`` () =
    let quizzer = QuizzerState.create "Jim"

    let initialQuiz =
        { quizWithQuizzerOnTeamOne quizzer with CurrentQuestion = PositiveNumber.one }

    let result =
        updateQuiz quizzer.Name initialQuiz

    assertSuccess result (fun updatedQuiz -> Assert.Equal(initialQuiz.CurrentQuestion |> PositiveNumber.increment, updatedQuiz.CurrentQuestion))