﻿module FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core

//Answer Question
type AnswerQuestion = Quizzer -> AnsweredQuestion

type UpdatedQuiz =
    { QuizState: RunningQuiz
      RevertedAnswer: RevertedCorrectAnswer }

type UpdateQuiz = Quizzer -> RunningQuiz -> Result<UpdatedQuiz, AnswerCorrectly.Error>

type CreateEvents = Quizzer -> UpdatedQuiz -> AnswerCorrectly.Event list

let private recordAnsweredQuestion quizzer currentQuestion (initialQuestionState) =
    initialQuestionState
    |> QuizAnswer.answerCorrectly quizzer currentQuestion

let updateQuizLevelInfo quizzer (quiz: RunningQuiz) =
    result {
        let newCurrentQuestion =
            quiz.CurrentQuestion |> PositiveNumber.increment

        let! updatedAnswer, revertedQuizzer =
            quiz.Questions
            |> Map.tryFind quiz.CurrentQuestion
            |> Option.map (fun q -> q.AnswerState)
            |> recordAnsweredQuestion quizzer quiz.CurrentQuestion
            |> Result.mapError (fun error ->
                error
                |> AnswerCorrectly.Error.QuizzerAlreadyAnsweredCorrectly)

        let currentQuestionInQuiz =
            { quiz with Questions = RunningQuiz.changeCurrentAnswer quiz updatedAnswer }
            |> changeCurrentQuestionInQuiz newCurrentQuestion

        return currentQuestionInQuiz, revertedQuizzer
    }


let updateQuiz: UpdateQuiz =
    fun quizzerName quiz ->
        result {

            let! updatedQuizInfo, revertedAnswer = updateQuizLevelInfo quizzerName quiz

            let revertedOpt =
                revertedAnswer |> RevertedCorrectAnswer.toOption

            let! updatedQuizWithScores =
                RunningQuiz.updateScoresBasedOnQuizzer QuizScore.correctAnswer quizzerName updatedQuizInfo
                |> Result.ofOption (AnswerCorrectly.QuizzerNotFound quizzerName)

            return
                revertedOpt
                |> Option.bind (fun reverted ->
                    RunningQuiz.updateScoresBasedOnQuizzer QuizScore.revertCorrectAnswer reverted updatedQuizWithScores)
                |> Option.defaultValue updatedQuizWithScores
                |> fun quiz ->
                    { QuizState = quiz
                      RevertedAnswer = revertedAnswer }
        }


let createEvents: CreateEvents =
    fun quizzer updatedQuiz ->
        let quizState = updatedQuiz.QuizState
        
        let answerer, teamUpdatedOpt =
            RunningQuiz.findQuizzer quizzer quizState

        let answererScoreChanged =
            AnswerCorrectly.Event.IndividualScoreChanged
                { NewScore = answerer.Score
                  Quiz = quizState.Code
                  Quizzer = quizzer
                  Question = quizState.CurrentQuestion }

        let revertedScoresChanged =
            match updatedQuiz.RevertedAnswer with
            | NoChange -> []
            | Reverted revertedQuizzer ->
                let revertedState, revertedTeamOpt =
                    RunningQuiz.findQuizzer revertedQuizzer quizState

                let scoreChanged =
                    { NewScore = revertedState.Score
                      Quiz = quizState.Code
                      Quizzer = revertedQuizzer
                      Question = quizState.CurrentQuestion }
                    |> AnswerCorrectly.Event.IndividualScoreChanged

                let teamScoreChanged =
                    Option.map2
                        (fun teamUpdated teamReverted ->
                            if teamUpdated <> teamReverted then
                                [ AnswerCorrectly.Event.TeamScoreChanged
                                      { NewScore = quizState |> RunningQuiz.getTeam teamReverted |> fun t -> t.Score
                                        Team = teamReverted
                                        Quiz = quizState.Code } ]
                            else
                                [])
                        teamUpdatedOpt
                        revertedTeamOpt
                    |> Option.defaultValue []

                [ scoreChanged
                  yield! teamScoreChanged ]

        let teamScoreChanged =
            teamUpdatedOpt
            |> Option.map (fun teamUpdated ->
                [ AnswerCorrectly.Event.TeamScoreChanged
                      { NewScore = quizState |> RunningQuiz.getTeam teamUpdated |> fun t -> t.Score
                        Team = teamUpdated
                        Quiz = quizState.Code } ])
            |> Option.defaultValue []


        let currentQuestionChanged =
            AnswerCorrectly.Event.CurrentQuestionChanged
                { Quiz = quizState.Code
                  NewQuestion = quizState.CurrentQuestion }

        [ yield! teamScoreChanged
          answererScoreChanged
          currentQuestionChanged
          yield! revertedScoresChanged ]

let answerCorrectly getQuiz saveQuiz : AnswerCorrectly.Workflow =
    fun command ->
        asyncResult {
            let! quiz =
                getQuiz command.Quiz
                |> AsyncResult.mapError AnswerCorrectly.Error.DbError
                |> AsyncResult.bind (fun quiz ->
                    quiz
                    |> Common.Pipeline.validateRunningQuiz
                    |> AsyncResult.ofResult
                    |> AsyncResult.mapError AnswerCorrectly.Error.QuizStateError)

            let! currentQuizzer =
                quiz
                |> validateCurrentQuizzer
                |> Result.mapError (fun e -> AnswerCorrectly.Error.NoCurrentQuizzer)
                |> AsyncResult.ofResult

            let! updatedQuiz =
                updateQuiz currentQuizzer quiz
                |> AsyncResult.ofResult

            do!
                updatedQuiz.QuizState
                |> Quiz.Running
                |> saveQuiz
                |> AsyncResult.mapError AnswerCorrectly.Error.DbError

            return createEvents currentQuizzer updatedQuiz
        }
