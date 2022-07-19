module FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline

open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow.AnswerIncorrectly
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

type ValidCurrentQuizzer = private ValidCurrentQuizzer of Quizzer
type ValidateCurrentQuizzer = RunningTeamQuiz -> Result<ValidCurrentQuizzer, AnswerIncorrectly.Error>
type UpdateQuiz = ValidCurrentQuizzer -> RunningTeamQuiz -> RunningTeamQuiz
type CreateEvents = RunningTeamQuiz -> AnswerIncorrectly.Event list

let validateQuizzer: ValidateCurrentQuizzer =
    fun quiz ->
        quiz.CurrentQuizzer
        |> Option.map ValidCurrentQuizzer
        |> (Result.ofOption Error.NoCurrentQuizzer)

let updateQuiz: UpdateQuiz =
    fun (ValidCurrentQuizzer _) quiz -> { quiz with CurrentQuizzer = None }

let createEvents: CreateEvents =
    fun quiz ->
        let quizzerChanged =
            { Quiz = quiz.Code
              CurrentQuizzer = quiz.CurrentQuizzer }
            |> Event.CurrentQuizzerChanged

        [ quizzerChanged ]

let answerIncorrectly getQuiz saveQuiz: Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.ofAsync
            let! runningQuiz = validateQuiz quiz |> AsyncResult.ofResult |> AsyncResult.mapError QuizState
            let! validQuizzer = validateQuizzer runningQuiz |> AsyncResult.ofResult
            let updatedQuiz = updateQuiz validQuizzer runningQuiz
            do! updatedQuiz |> Running |> saveQuiz |> AsyncResult.ofAsync
            return createEvents updatedQuiz
        }
