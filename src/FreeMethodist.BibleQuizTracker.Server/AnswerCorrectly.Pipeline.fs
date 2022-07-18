module FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline

open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow.AnswerCorrectly
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core

//Answer Question
type AnswerQuestion = Quizzer -> AnsweredQuestion
type UpdateQuiz = Quizzer -> RunningTeamQuiz -> Result<RunningTeamQuiz, AnswerCorrectly.Error>

type CreateEvents = RunningTeamQuiz -> RunningTeamQuiz -> AnswerCorrectly.Event list


let updateQuiz : UpdateQuiz =
    fun quizzer quiz ->
        quizzer |> Error.QuizzerNotFound |> Result.Error