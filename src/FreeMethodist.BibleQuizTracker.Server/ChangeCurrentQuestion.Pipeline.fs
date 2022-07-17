module FreeMethodist.BibleQuizTracker.Server.ChangeCurrentQuestion_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Pipeline

type CreateEvent = RunningTeamQuiz -> CurrentQuestionChanged

let updateQuiz quiz question =
    { quiz with CurrentQuestion = question }

let createEvent (quiz: RunningTeamQuiz) =
    { Quiz = quiz.Code
      NewQuestion = quiz.CurrentQuestion }

let changeCurrentQuestion (getQuiz: GetTeamQuiz) (saveQuiz: SaveTeamQuiz) : ChangeCurrentQuestion.WorkflowDeprecated =
    fun command ->
        result {
            let quiz = getQuiz command.Quiz
            let! runningQuiz = validateQuiz quiz

            let newQuizState =
                updateQuiz runningQuiz command.Data.Question

            saveQuiz (Running newQuizState)
            return createEvent newQuizState
        }

let changeCurrentQuestionAsync
    (getQuiz: GetTeamQuizAsync)
    (saveQuiz: SaveTeamQuizAsync)
    : ChangeCurrentQuestion.Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.ofAsync
            let! runningQuiz = quiz |> validateQuiz |> AsyncResult.ofResult

            let newQuizState =
                updateQuiz runningQuiz command.Data.Question

            do!
                newQuizState
                |> Running
                |> saveQuiz
                |> AsyncResult.ofAsync

            return createEvent newQuizState
        }
