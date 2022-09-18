module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerCorrectly.CreateEventsStepTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Tests.Quiz
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

        let initialQuiz = RunningQuiz.identity

        let! quizQuestion, _ =
            (Some QuizAnswer.initial
             |> QuizAnswer.answerCorrectly "previous" initialQuiz.CurrentQuestion)

        let setupQuiz (quiz: RunningQuiz) : UpdatedQuiz =
            { QuizState =
                { quiz with
                    TeamOne = { quiz.TeamOne with Quizzers = [ previousAnswerer; newAnswerer ] } }
              RevertedAnswer = Reverted previousAnswerer.Name }

        let events =
            initialQuiz
            |> setupQuiz
            |> fun quiz -> { quiz with QuizState = insertCurrentAnswer quizQuestion quiz.QuizState }
            |> createEvents newAnswerer.Name

        let expectedEvent: IndividualScoreChanged =
            { Quiz = initialQuiz.Code
              Quizzer = previousAnswerer.Name
              NewScore = previousAnswerer.Score
              Question = initialQuiz.CurrentQuestion }

        Assert.Contains(AnswerCorrectly.Event.IndividualScoreChanged expectedEvent, events)
    }

[<Fact>]
let ``Creates Answer Correctly events for Individual Quiz`` () =
    let newAnswerer =
            QuizzerState.create "answerer"

    let initialQuiz = RunningQuiz.identity
            
    let eventsResult = result {
        let! quizQuestion, _ =
            (Some QuizAnswer.initial
             |> QuizAnswer.answerCorrectly "previous" initialQuiz.CurrentQuestion)

        let setupQuiz (quiz: RunningQuiz) : UpdatedQuiz =
            { QuizState =
                { quiz with CompetitionStyle = RunningCompetitionStyle.Individuals [newAnswerer] }
              RevertedAnswer = NoChange }
        return
            initialQuiz
            |> setupQuiz
            |> fun quiz -> { quiz with QuizState = insertCurrentAnswer quizQuestion quiz.QuizState }
            |> createEvents newAnswerer.Name
    }
     
    let expectedEvent =  
            { Quiz = initialQuiz.Code
              Quizzer = newAnswerer.Name
              NewScore = newAnswerer.Score
              Question = initialQuiz.CurrentQuestion }   
    match eventsResult with
    | Error error -> failwith $"error: {error}"
    | Ok events -> Assert.Contains(AnswerCorrectly.Event.IndividualScoreChanged expectedEvent, events)