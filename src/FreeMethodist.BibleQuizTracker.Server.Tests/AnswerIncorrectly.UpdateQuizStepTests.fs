module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerIncorrectly_UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Tests.Common
open Xunit

[<Fact>]
let ``When Answered Incorrectly record Question with incorrect answerer`` () =
    let answerer = QuizzerState.create "Jim"

    let initialQuiz =
        { RunningTeamQuiz.identity with
            Questions = Map.empty
            CurrentQuizzer = (Some answerer.Name) }

    let result =
        validateQuizzer initialQuiz
        |> Result.bind (fun q -> updateQuiz q initialQuiz)

    assertSuccess result (fun quiz ->
        let question =
            quiz.Questions[quiz.CurrentQuestion]

        let expectedQuestion =
            Incomplete [ answerer.Name ]

        Assert.Equal(expectedQuestion, question))

[<Fact>]
let ``Given Quizzer was recorded answering correctly for question earlier When Answered Incorrectly then decrement score``
    ()
    =
    let answerer = QuizzerState.create "Jim"

    let previouslyAnsweredQuestion =
        ({ Answerer = answerer.Name
           IncorrectAnswerers = [] }
         |> Answered
         |> Complete)

    let initialQuiz =
        { RunningTeamQuiz.identity with
            CurrentQuizzer = (Some answerer.Name)
            TeamOne = { RunningTeamQuiz.identity.TeamOne with Quizzers = [ answerer ] }
            Questions =
                Map.empty
                |> Map.add RunningTeamQuiz.identity.CurrentQuestion previouslyAnsweredQuestion }

    let result =
        validateQuizzer initialQuiz
        |> Result.bind (fun q -> updateQuiz q initialQuiz)

    assertSuccess result (fun quiz ->
        let quizzerState =
            quiz.TeamOne.Quizzers
            |> List.find (fun q -> q.Name = answerer.Name)

        let expectedScore =
            answerer.Score |> TeamScore.revertCorrectAnswer

        Assert.Equal(expectedScore, quizzerState.Score))

[<Fact>]
let ``Given Quizzer was recorded answering incorrectly for an answered question earlier When Answered Incorrectly then Error``
    ()
    =
    let answerer = QuizzerState.create "Jim"

    let previouslyAnsweredQuestion =
        ({ Answerer = "Different"
           IncorrectAnswerers = [ answerer.Name ] }
         |> Answered
         |> Complete)

    let initialQuiz =
        { RunningTeamQuiz.identity with
            CurrentQuizzer = (Some answerer.Name)
            TeamOne = { RunningTeamQuiz.identity.TeamOne with Quizzers = [ answerer ] }
            Questions =
                Map.empty
                |> Map.add RunningTeamQuiz.identity.CurrentQuestion previouslyAnsweredQuestion }

    let result =
        validateQuizzer initialQuiz
        |> Result.bind (fun q -> updateQuiz q initialQuiz)

    let expectedResult =
        AnswerIncorrectly.Workflow.AnswerIncorrectly.Error.QuizzerAlreadyAnsweredIncorrectly(
            answerer.Name,
            initialQuiz.CurrentQuestion
        )
        |> Result.Error

    Assert.Equal(expectedResult, result)

[<Fact>]
let ``Given Quizzer was recorded answering incorrectly for an unanswered question earlier When Answered Incorrectly then Error``
    ()
    =
    let answerer = QuizzerState.create "Jim"

    let previouslyUnansweredQuestion =
        ([ answerer.Name ] |> Unanswered |> Complete)

    let initialQuiz =
        { RunningTeamQuiz.identity with
            CurrentQuizzer = (Some answerer.Name)
            TeamOne = { RunningTeamQuiz.identity.TeamOne with Quizzers = [ answerer ] }
            Questions =
                Map.empty
                |> Map.add RunningTeamQuiz.identity.CurrentQuestion previouslyUnansweredQuestion }

    let result =
        validateQuizzer initialQuiz
        |> Result.bind (fun q -> updateQuiz q initialQuiz)

    let expectedResult =
        AnswerIncorrectly.Workflow.AnswerIncorrectly.Error.QuizzerAlreadyAnsweredIncorrectly(
            answerer.Name,
            initialQuiz.CurrentQuestion
        )
        |> Result.Error

    Assert.Equal(expectedResult, result)
