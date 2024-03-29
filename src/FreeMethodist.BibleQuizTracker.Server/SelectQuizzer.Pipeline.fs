﻿module FreeMethodist.BibleQuizTracker.Server.SelectQuizzer_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow

type ValidateSelection = Quiz -> SelectQuizzer.Input -> Result<RunningQuiz, SelectQuizzer.Error>
type ChangeCurrentQuizzer = Quizzer -> RunningQuiz -> RunningQuiz
type CreateEvent = RunningQuiz -> Quizzer -> CurrentQuizzerChanged


let validateSelection: ValidateSelection =
    fun teamQuiz input ->
        result {
            let! validQuiz =
                validateRunningQuiz teamQuiz
                |> Result.mapError SelectQuizzer.Error.QuizState

            let quizzers =
                match validQuiz.CompetitionStyle with
                | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
                    teamOne.Quizzers
                    @ teamTwo.Quizzers
                | RunningCompetitionStyle.Individuals quizzerStates -> quizzerStates

            let hasQuizzer =
                quizzers
                |> List.exists (fun q -> q.Name = input.Quizzer)


            do!
                if hasQuizzer then
                    Ok()
                else
                    Error(SelectQuizzer.Error.QuizzerNotParticipating input.Quizzer)

            return!
                match validQuiz.CurrentQuizzer with
                | None -> Ok validQuiz
                | Some currentQuizzer ->
                    if currentQuizzer = input.Quizzer then
                        Error SelectQuizzer.QuizzerAlreadyCurrent
                    else
                        Ok validQuiz
        }

let changeCurrentQuizzer: ChangeCurrentQuizzer =
    fun quizzer quiz -> { quiz with CurrentQuizzer = Some quizzer }

let createEvent: CreateEvent =
    fun quiz quizzer ->
        { Quiz = quiz.Code
          CurrentQuizzer = Some quizzer }

let selectQuizzer getQuiz (saveQuiz: SaveQuiz) : SelectQuizzer.Workflow =
    fun command ->
        let validateSelection quiz =
            validateSelection quiz command.Data
            |> AsyncResult.ofResult

        asyncResult {
            let! validQuiz =
                getQuiz command.Quiz
                |> AsyncResult.mapError SelectQuizzer.DbError
                |> AsyncResult.bind validateSelection

            do!
                validQuiz
                |> changeCurrentQuizzer command.Data.Quizzer
                |> Quiz.Running
                |> saveQuiz
                |> AsyncResult.mapError SelectQuizzer.DbError

            return createEvent validQuiz command.Data.Quizzer
        }
