module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerIncorrectly.CreateEventsStepTests

open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Tests.Quiz
open Xunit

[<Fact>]
let ``Given current quizzer answered current question correctly When current quizzer answers incorrectly then publish a IndividualScoreChanged event``
    ()
    =
    result {

        let answerer =
            QuizzerState.create "answerer"

        let initialQuiz = RunningQuiz.newTeamQuiz

        let! quizQuestion, _ =
            (Some QuizAnswer.initial
             |> QuizAnswer.answerCorrectly answerer.Name initialQuiz.CurrentQuestion)

        let setupQuiz (quiz: RunningQuiz) : UpdatedQuiz =
            { QuizState =
                quiz
                |> Arrange.withParticipants [ answerer ]
                |> insertCurrentAnswer quizQuestion
              RevertedAnswer = Reverted answerer.Name }

        let events =
            initialQuiz |> setupQuiz |> createEvents

        let expectedEvent: IndividualScoreChanged =
            { Quiz = initialQuiz.Code
              Quizzer = answerer.Name
              NewScore = answerer.Score
              Question = initialQuiz.CurrentQuestion }

        Assert.Contains(AnswerIncorrectly.Event.IndividualScoreChanged expectedEvent, events)
    }

[<Fact>]
let ``Given current quizzer answered current question correctly When current quizzer answers incorrectly then publish a TeamScoreChanged event``
    ()
    =
    result {

        let answerer =
            QuizzerState.create "answerer"

        let initialQuiz = RunningQuiz.newTeamQuiz

        let! quizAnswer, _ =
            (Some QuizAnswer.initial
             |> QuizAnswer.answerCorrectly answerer.Name initialQuiz.CurrentQuestion)

        let setupQuiz (quiz: RunningQuiz) : UpdatedQuiz =
            { QuizState =
                quiz
                |> Arrange.withParticipants [answerer]
                |> insertCurrentAnswer quizAnswer
              RevertedAnswer = Reverted answerer.Name }

        let events =
            initialQuiz |> setupQuiz |> createEvents

        let expectedEvent: TeamScoreChanged =
            { Quiz = initialQuiz.Code
              NewScore = answerer.Score
              Team = TeamOne }

        Assert.Contains(AnswerIncorrectly.Event.TeamScoreChanged expectedEvent, events)
    }
