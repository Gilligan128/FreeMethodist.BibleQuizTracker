module FreeMethodist.BibleQuizTracker.Server.ListQuizzes.Persistence

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Persistence_CosmosDb
open FreeMethodist.BibleQuizTracker.Server.QuizState_Persistence_CosmosDb

type ListQuizInput = { Status : QuizStatusFilter  }

let getQuizzes tenant cosmosClient input =
       asyncResult {
            let! database = cosmosClient |> ensureBibleQuizDatabase tenant

            let! containerClient =
                database.Database
                |> ensureQuizContainer
                |> AsyncResult.map (fun container -> container.Container)

            let quizzes =
                containerClient.GetItemLinqQueryable<QuizEntity>(true, null, null)
                |> Seq.where (fun quiz ->
                    match input.Status, quiz.data with
                    | All, _ -> true
                    | QuizStatusFilter.Running, Quiz.Running _ -> true
                    | QuizStatusFilter.Completed, Quiz.Completed _ -> true
                    | QuizStatusFilter.Official, Quiz.Official _ -> true
                    | _ -> false)
                |> Seq.map (fun quiz -> quiz.data)
                
            return quizzes
        }