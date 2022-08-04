module FreeMethodist.BibleQuizTracker.Server.CreateQuiz.Pipeline

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.CreateQuiz.Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow


let createQuiz (SaveNewQuiz saveQuiz) : CreateQuiz.Workflow =
    fun command ->
        asyncResult {
            let! runningQuiz =
                Quiz.start command
                |> Result.mapError (fun _ -> CreateQuiz.Error.IndividualCompetitionStyle)
                |> AsyncResult.ofResult

            do!
                runningQuiz
                |> saveQuiz
                |> AsyncResult.mapError CreateQuiz.DbError

            return [ CreateQuiz.Event.QuizCreated command.Code ]
        }
