﻿module FreeMethodist.BibleQuizTracker.Server.SelectQuizzer_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Pipeline
open FreeMethodist.BibleQuizTracker.Server.SelectQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

type ValidateSelection = TeamQuiz -> SelectQuizzer.Input -> Result<RunningTeamQuiz, SelectQuizzer.Error>
type ChangeCurrentQuizzer = Quizzer -> RunningTeamQuiz  -> RunningTeamQuiz
type CreateEvent = RunningTeamQuiz -> Quizzer -> CurrentQuizzerChanged


let validateSelection: ValidateSelection =
    fun teamQuiz input ->
        result {
            let! validQuiz =
                validateQuiz teamQuiz
                |> Result.mapError SelectQuizzer.Error.QuizState

            let hasQuizzer =
                validQuiz.TeamOne.Quizzers
                @ validQuiz.TeamTwo.Quizzers
                |> List.exists (fun q -> q.Name = input.Quizzer)

            do!
                if hasQuizzer then
                    Ok()
                else
                    Error(SelectQuizzer.Error.QuizzerNotParticipating input.Quizzer)

            return!
                if validQuiz.CurrentQuizzer = input.Quizzer then
                    Error SelectQuizzer.QuizzerAlreadyCurrent
                else
                    Ok validQuiz
        }

let changeCurrentQuizzer : ChangeCurrentQuizzer =
    fun quizzer quiz ->
        { quiz with CurrentQuizzer = quizzer} 
        
let createEvent : CreateEvent =
    fun quiz quizzer -> { Quiz = quiz.Code; Quizzer = quizzer }
    
 
let selectQuizzer getQuiz (saveQuiz: SaveTeamQuiz ): SelectQuizzer.Workflow =
    fun command ->
        let validateSelection quiz = validateSelection quiz command.Data
        result {
            let! validQuiz = getQuiz command.Quiz
                                |> validateSelection
            validQuiz |> changeCurrentQuizzer command.Data.Quizzer |> Running |> saveQuiz 
            return createEvent validQuiz command.Data.Quizzer 
        }