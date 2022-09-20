module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerIncorrectly_UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Tests.Quiz
open Xunit

[<Fact>]
let ``When Answered Incorrectly record Question with incorrect answerer`` () =
    let answerer = QuizzerState.create "Jim"

    let answer =
        result {

            let initialQuiz =
                { RunningQuiz.newTeamQuiz with CurrentQuizzer = (Some answerer.Name) }
                |> Arrange.withParticipants [ answerer ]

            let! result = updateQuiz answerer.Name initialQuiz

            return
                result.QuizState.Questions[result.QuizState.CurrentQuestion]
                    .AnswerState

        }

    let expectedQuestion =
        Incomplete [ answerer.Name ]

    match answer with
    | Ok answer -> Assert.Equal(expectedQuestion, answer)
    | Error error -> failwith $"error"


[<Fact>]
let ``Given Quizzer was recorded answering correctly for question earlier When Answered Incorrectly then decrement score``
    ()
    =
    result {
        let answerer = QuizzerState.create "Jim"

        let previouslyAnsweredQuestion =
            ({ Answerer = answerer.Name
               IncorrectAnswerers = [] }
             |> Answered
             |> Complete)

        let setupQuiz quiz =
            { quiz with CurrentQuizzer = (Some answerer.Name) }
            |> Arrange.withParticipants [ answerer ]

        let initialQuiz =
            RunningQuiz.newTeamQuiz
            |> setupQuiz
            |> insertCurrentAnswer previouslyAnsweredQuestion

        let! quiz = updateQuiz answerer.Name initialQuiz

        let quizzerState, _ =
            quiz.QuizState
            |> RunningQuiz.findQuizzer answerer.Name

        let expectedScore =
            answerer.Score |> QuizScore.revertCorrectAnswer

        Assert.Equal(expectedScore, quizzerState.Score)

        Assert.True(
            (initialQuiz
             |> RunningQuiz.getTeamScore TeamPosition.TeamOne) > (quiz.QuizState
                                                                  |> RunningQuiz.getTeamScore TeamPosition.TeamOne)
        )
    }

[<Fact>]
let ``Given Quizzer was recorded answering correctly for question earlier When Answered Incorrectly then quizzer is no longer answerer``
    ()
    =
    result {
        let answerer = QuizzerState.create "Jim"

        let previouslyAnsweredQuestion =
            ({ Answerer = answerer.Name
               IncorrectAnswerers = [] }
             |> Answered
             |> Complete)

        let setupQuiz quiz =
            { quiz with CurrentQuizzer = (Some answerer.Name) }
            |> Arrange.withParticipants [ answerer ]

        let initialQuiz =
            RunningQuiz.newTeamQuiz
            |> setupQuiz
            |> insertCurrentAnswer previouslyAnsweredQuestion

        let! result = updateQuiz answerer.Name initialQuiz

        let question =
            result.QuizState.Questions[result.QuizState.CurrentQuestion]

        Assert.Equal([ answerer.Name ] |> Unanswered |> Complete, question.AnswerState)
    }

[<Fact>]
let ``Given Quizzer was recorded answering incorrectly for an answered question earlier When Answered Incorrectly then Error``
    ()
    =
    let answerer = QuizzerState.create "Jim"

    let previouslyAnswered =
        ({ Answerer = "Different"
           IncorrectAnswerers = [ answerer.Name ] }
         |> Answered
         |> Complete)

    let initialQuiz =
        { RunningQuiz.newTeamQuiz with CurrentQuizzer = (Some answerer.Name) }
        |> Arrange.withParticipants [ answerer ]
        |> insertCurrentAnswer previouslyAnswered

    let result =
        updateQuiz answerer.Name initialQuiz

    let expectedResult =
        QuizAnswer.QuizzerAlreadyAnsweredIncorrectly(answerer.Name, initialQuiz.CurrentQuestion)
        |> AnswerIncorrectly.Error.QuizzerAlreadyAnsweredIncorrectly
        |> Result.Error

    Assert.Equal(expectedResult, result)

[<Fact>]
let ``Given Quizzer was recorded answering incorrectly for an unanswered question earlier When Answered Incorrectly then Error``
    ()
    =
    let answerer = QuizzerState.create "Jim"

    let previouslyUnanswered =
        ([ answerer.Name ] |> Unanswered |> Complete)

    let initialQuiz =
        { RunningQuiz.newTeamQuiz with CurrentQuizzer = (Some answerer.Name) }
        |> Arrange.withParticipants [ answerer ]
        |> insertCurrentAnswer previouslyUnanswered

    let result =
        updateQuiz answerer.Name initialQuiz

    let expectedResult =
        QuizAnswer.QuizzerAlreadyAnsweredIncorrectly(answerer.Name, initialQuiz.CurrentQuestion)
        |> AnswerIncorrectly.Error.QuizzerAlreadyAnsweredIncorrectly
        |> Result.Error

    Assert.Equal(expectedResult, result)
