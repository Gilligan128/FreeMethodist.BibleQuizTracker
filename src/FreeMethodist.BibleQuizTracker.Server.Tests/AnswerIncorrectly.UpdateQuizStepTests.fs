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
        |> Result.map (fun q -> updateQuiz q initialQuiz)

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
        |> Result.map (fun q -> updateQuiz q initialQuiz)

    assertSuccess result (fun quiz ->
        let quizzerState =
            quiz.TeamOne.Quizzers
            |> List.find (fun q -> q.Name = answerer.Name)

        let expectedScore =
            answerer.Score |> TeamScore.revertCorrectAnswer

        Assert.Equal(expectedScore, quizzerState.Score))
