module FreeMethodist.BibleQuizTracker.Server.ChangeCurrentQuestion_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

type CreateEvent = RunningTeamQuiz -> CurrentQuestionChanged


let createEvent (quiz: RunningTeamQuiz) =
    { Quiz = quiz.Code
      NewQuestion = quiz.CurrentQuestion }

let changeCurrentQuestionAsync
    (getQuiz: GetQuiz)
    (saveQuiz: SaveQuiz)
    : ChangeCurrentQuestion.Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.mapError ChangeCurrentQuestion.DbError

            let! runningQuiz =
                quiz
                |> validateQuiz
                |> AsyncResult.ofResult
                |> AsyncResult.mapError ChangeCurrentQuestion.QuizState

            let newQuizState =
                changeCurrentQuestionInQuiz  command.Data.Question runningQuiz

            do!
                newQuizState
                |> Running
                |> saveQuiz
                |> AsyncResult.mapError ChangeCurrentQuestion.Error.DbError

            return createEvent newQuizState
        }
