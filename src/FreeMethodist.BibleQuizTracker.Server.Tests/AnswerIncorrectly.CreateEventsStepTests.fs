module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerIncorrectly.CreateEventsStepTests

open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline
open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit

[<Fact>]
let ``Given current quizzer answered current question correctly When current quizzer answers incorrectly then publish a IndividualScoreChanged event``
    ()
    =
    result {

        let answerer =
            QuizzerState.create "answerer"

        let initialQuiz = RunningTeamQuiz.identity

        let! quizQuestion, _ =
            (Some QuizAnswer.create
             |> QuizAnswer.answerCorrectly answerer.Name initialQuiz.CurrentQuestion)

        let setupQuiz (quiz: RunningTeamQuiz) : UpdatedQuiz =
            { QuizState =
                { quiz with
                    QuestionsDeprecated =
                        quiz.QuestionsDeprecated
                        |> Map.add quiz.CurrentQuestion quizQuestion
                    TeamOne = { quiz.TeamOne with Quizzers = [ answerer ] } }
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

        let initialQuiz = RunningTeamQuiz.identity

        let! quizQuestion, _ =
            (Some QuizAnswer.create
             |> QuizAnswer.answerCorrectly answerer.Name initialQuiz.CurrentQuestion)

        let setupQuiz (quiz: RunningTeamQuiz) : UpdatedQuiz =
            { QuizState =
                { quiz with
                    QuestionsDeprecated =
                        quiz.QuestionsDeprecated
                        |> Map.add quiz.CurrentQuestion quizQuestion
                    TeamOne = { quiz.TeamOne with Quizzers = [ answerer ] } }
              RevertedAnswer = Reverted answerer.Name }

        let events =
            initialQuiz |> setupQuiz |> createEvents

        let expectedEvent: TeamScoreChanged =
            { Quiz = initialQuiz.Code
              NewScore = answerer.Score
              Team = TeamOne }

        Assert.Contains(AnswerIncorrectly.Event.TeamScoreChanged expectedEvent, events)
    }
