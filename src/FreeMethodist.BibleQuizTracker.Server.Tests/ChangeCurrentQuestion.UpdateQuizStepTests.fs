module FreeMethodist.BibleQuizTracker.Server.Tests.ChangeCurrentQuestion_UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Tests.Quiz
open Xunit
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

[<Fact>]
let ``Given this is the first time a question is current When Changing Question then record an Unanswered Question for previous Question``
    ()
    =
    let initialQuiz = RunningQuiz.identity

    let nextQuestion =
        initialQuiz.CurrentQuestion
        |> PositiveNumber.increment

    let result =
        changeCurrentQuestionInQuiz nextQuestion initialQuiz

    let expectedQuestion =
        [] |> Unanswered |> Complete

    Assert.Equal(
        expectedQuestion,
        result.Questions[initialQuiz.CurrentQuestion]
            .AnswerState
    )


[<Fact>]
let ``Given this is not the first time a question is current When Changing Question then leave the Question untouched``
    ()
    =
    let quizAnswer =
        { Answerer = "Jim"
          IncorrectAnswerers = [] }
        |> Answered
        |> Complete

    let initialQuiz =
        RunningQuiz.identity
        |> insertCurrentAnswer quizAnswer

    let nextQuestion =
        initialQuiz.CurrentQuestion
        |> PositiveNumber.increment

    let result =
        changeCurrentQuestionInQuiz  nextQuestion initialQuiz

    Assert.Equal(
        quizAnswer,
        result.Questions[initialQuiz.CurrentQuestion]
            .AnswerState
    )

[<Fact>]
let ``Given the next current question is brand new  When Changing Question then record an Incomplete next question`` () =
    let initialQuiz = RunningQuiz.identity

    let nextQuestion =
        initialQuiz.CurrentQuestion
        |> PositiveNumber.increment

    let result =
        changeCurrentQuestionInQuiz nextQuestion initialQuiz

    let expectedQuestion = QuestionState.initial
    Assert.Equal(expectedQuestion, result.Questions[result.CurrentQuestion])

[<Fact>]
let ``Given the next current question is not new When Changing Question then retain existing next question state`` () =
    let quizAnswer =
        { Answerer = "Jim"
          IncorrectAnswerers = [] }
        |> Answered
        |> Complete

    let nextQuestion =
        PositiveNumber.one
        |> PositiveNumber.increment
    
    let initialQuiz =
        RunningQuiz.identity
        |> fun quiz -> {quiz with CurrentQuestion = PositiveNumber.one}
        |> insertAnswer nextQuestion quizAnswer

    let result =
        changeCurrentQuestionInQuiz  nextQuestion initialQuiz

    Assert.Equal(quizAnswer, result.Questions[result.CurrentQuestion].AnswerState)
