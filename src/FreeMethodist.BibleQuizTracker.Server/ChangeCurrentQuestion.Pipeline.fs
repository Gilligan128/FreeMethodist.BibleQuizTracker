module FreeMethodist.BibleQuizTracker.Server.ChangeCurrentQuestion_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

type CreateEvent = RunningTeamQuiz -> CurrentQuestionChanged

let updateQuiz quiz question =
    { quiz with
        CurrentQuestion = question
        QuestionsDeprecated = 
            quiz.QuestionsDeprecated
            |> Map.change question (fun questionOpt ->
                questionOpt
                |> Option.defaultValue ([] |> Unanswered |> Complete)
                |> Some) }

let createEvent (quiz: RunningTeamQuiz) =
    { Quiz = quiz.Code
      NewQuestion = quiz.CurrentQuestion }

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
