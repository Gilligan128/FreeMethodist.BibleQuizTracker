module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerIncorrectly_UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Tests.Common
open Xunit

[<Fact>]
let ``When Answered Incorrectly record Question with incorrect answerer`` () =
    result {
        let answerer = QuizzerState.create "Jim"

        let initialQuiz =
            { RunningTeamQuiz.identity with
                QuestionsDeprecated = Map.empty
                CurrentQuizzer = (Some answerer.Name) }

        let! result =
            updateQuiz answerer.Name initialQuiz

        let questionDeprecated =
            result.QuizState.QuestionsDeprecated[result.QuizState.CurrentQuestion]

        let answer =
            result.QuizState.Questions[result.QuizState.CurrentQuestion]
                .AnswerState

        let expectedQuestion =
            Incomplete [ answerer.Name ]

        Assert.Equal(expectedQuestion, questionDeprecated)
        Assert.Equal(expectedQuestion, answer)
    }

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
            QuestionsDeprecated =
                Map.empty
                |> Map.add RunningTeamQuiz.identity.CurrentQuestion previouslyAnsweredQuestion }

    let result =
        updateQuiz answerer.Name initialQuiz

    assertSuccess result (fun quiz ->
        let quizzerState =
            quiz.QuizState.TeamOne.Quizzers
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
            QuestionsDeprecated =
                Map.empty
                |> Map.add RunningTeamQuiz.identity.CurrentQuestion previouslyAnsweredQuestion }

    let result =
        updateQuiz answerer.Name initialQuiz

    let expectedResult =
        QuizAnswer.QuizzerAlreadyAnsweredIncorrectly(answerer.Name, initialQuiz.CurrentQuestion)
        |> AnswerIncorrectly.Workflow.AnswerIncorrectly.QuizzerAlreadyAnsweredIncorrectly
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
            QuestionsDeprecated =
                Map.empty
                |> Map.add RunningTeamQuiz.identity.CurrentQuestion previouslyUnansweredQuestion }

    let result =
        updateQuiz answerer.Name initialQuiz

    let expectedResult =
        QuizAnswer.QuizzerAlreadyAnsweredIncorrectly(answerer.Name, initialQuiz.CurrentQuestion)
        |> AnswerIncorrectly.Workflow.AnswerIncorrectly.QuizzerAlreadyAnsweredIncorrectly
        |> Result.Error

    Assert.Equal(expectedResult, result)
