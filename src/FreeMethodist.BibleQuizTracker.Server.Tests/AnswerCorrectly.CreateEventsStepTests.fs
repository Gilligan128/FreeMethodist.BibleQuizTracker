module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerCorrectly.CreateEventsStepTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit

[<Fact>]
let ``Given Question was answered correctly When current quizzer answers correctly then publish a IndividualScoreChanged event for previous answerer``
    ()
    =
    result {
        let previousAnswerer =
            QuizzerState.create "previous"

        let newAnswerer =
            QuizzerState.create "answerer"

        let initialQuiz = RunningTeamQuiz.identity

        let! quizQuestion, _ =
            (Some QuizAnswer.create
             |> QuizAnswer.answerCorrectly "previous" initialQuiz.CurrentQuestion)

        let setupQuiz (quiz: RunningTeamQuiz) : UpdatedQuiz =
            { QuizState =
                { quiz with
                    QuestionsDeprecated =
                        quiz.QuestionsDeprecated
                        |> Map.add quiz.CurrentQuestion quizQuestion
                    TeamOne = { quiz.TeamOne with Quizzers = [ previousAnswerer; newAnswerer ] } }
              RevertedAnswer = Reverted previousAnswerer.Name }

        let events =
            initialQuiz
            |> setupQuiz
            |> createEvents newAnswerer.Name

        let expectedEvent: IndividualScoreChanged =
            { Quiz = initialQuiz.Code
              Quizzer = previousAnswerer.Name
              NewScore = previousAnswerer.Score
              Question = initialQuiz.CurrentQuestion }

        Assert.Contains(AnswerCorrectly.Event.IndividualScoreChanged expectedEvent, events)
    }
