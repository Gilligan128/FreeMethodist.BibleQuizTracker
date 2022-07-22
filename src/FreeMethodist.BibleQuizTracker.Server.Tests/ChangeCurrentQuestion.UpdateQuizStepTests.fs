module FreeMethodist.BibleQuizTracker.Server.Tests.ChangeCurrentQuestion_UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Tests.Quiz
open Xunit

[<Fact>]
let ``Given this is the first time a question is current When Changing Question then record an Unanswered Question for previous Question``
    ()
    =
    let initialQuiz = RunningTeamQuiz.identity

    let nextQuestion =
        initialQuiz.CurrentQuestion
        |> PositiveNumber.increment

    let result =
        ChangeCurrentQuestion_Pipeline.updateQuiz initialQuiz nextQuestion

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
        RunningTeamQuiz.identity
        |> insertCurrentAnswer quizAnswer

    let nextQuestion =
        initialQuiz.CurrentQuestion
        |> PositiveNumber.increment

    let result =
        ChangeCurrentQuestion_Pipeline.updateQuiz initialQuiz nextQuestion

    Assert.Equal(quizAnswer, result.Questions[initialQuiz.CurrentQuestion].AnswerState)
