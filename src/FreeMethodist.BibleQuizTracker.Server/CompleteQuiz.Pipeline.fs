module FreeMethodist.BibleQuizTracker.Server.CompleteQuiz.Pipeline

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

let updateQuizToComplete quiz =
    quiz

let completeQuiz getQuiz saveQuiz : CompleteQuiz.Workflow =
    fun command ->
        asyncResult {
            let! quiz = command |> getQuiz

            let! validQuiz =
                quiz
                |> validateQuiz
                |> Result.mapError CompleteQuiz.QuizState
                |> AsyncResult.ofResult
            
            let updatedQuiz = validQuiz |> updateQuizToComplete
            
            return []
        }
