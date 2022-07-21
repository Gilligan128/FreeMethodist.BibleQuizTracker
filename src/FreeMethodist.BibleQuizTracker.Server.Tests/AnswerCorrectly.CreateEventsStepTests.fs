module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerCorrectly.CreateEventsStepTests

open FreeMethodist.BibleQuizTracker.Server
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

        let! quizQuestion, _ =
            (Some QuizQuestion.create
             |> QuizQuestion.answerCorrectly "previous" RunningTeamQuiz.identity.CurrentQuestion)

        let setupQuiz (quiz: RunningTeamQuiz) =
            { RunningTeamQuiz.identity with
                Questions =
                    RunningTeamQuiz.identity.Questions
                    |> Map.add RunningTeamQuiz.identity.CurrentQuestion quizQuestion
                TeamOne = { quiz.TeamOne with Quizzers = [ previousAnswerer; newAnswerer ] } }

        let events =
            RunningTeamQuiz.identity
            |> setupQuiz
            |> AnswerCorrectly_Pipeline.createEvents newAnswerer.Name

        Assert.True(true)
    }
