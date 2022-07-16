﻿module FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Pipeline

type MoveQuizToQuestion = Quiz -> Result<RunningTeamQuiz, QuizStateError>
type CreateEvent = RunningTeamQuiz  -> CurrentQuestionChanged


let updateQuiz quiz question =
   { quiz with CurrentQuestion = question }
let createEvent (quiz: RunningTeamQuiz)  =
    { Quiz = quiz.Code; NewQuestion = quiz.CurrentQuestion }

let moveQuizToQuestion (getQuiz:GetTeamQuiz) (saveQuiz:SaveTeamQuiz) : MoveQuestion.Workflow =
    fun command ->
        result {
            let quiz = getQuiz command.Quiz
            let! runningQuiz = validateQuiz quiz
            let newQuizState = updateQuiz runningQuiz command.Data.Question
            saveQuiz (Running newQuizState)
            return createEvent newQuizState
        }
