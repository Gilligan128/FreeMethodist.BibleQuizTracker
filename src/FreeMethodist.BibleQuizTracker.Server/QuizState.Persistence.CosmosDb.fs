module FreeMethodist.BibleQuizTracker.Server.QuizState_Persistence_CosmosDb

open System.Net
open System.Text.Json
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.Azure.Cosmos
open Persistence_CosmosDb

type QuizEntity = { id: string; data: Quiz }

let private getCodeFromQuiz quiz =
    match quiz with
    |  Quiz.Running runningTeamQuiz -> runningTeamQuiz.Code
    |  Quiz.Completed completedTeamQuiz -> completedTeamQuiz.Code
    |  Quiz.Official officialTeamQuiz -> officialTeamQuiz.Code

let private getBlobName quizCode = $"quiz-{quizCode}"

let saveNewQuiz (tenantName: string) (cosmosClient: CosmosClient) =
    fun quiz ->
        asyncResult {
            let! database = cosmosClient |> ensureBibleQuizDatabase tenantName

            let! containerClient =
                database.Database
                |> ensureQuizContainer
                |> AsyncResult.map (fun container -> container.Container)

            do!
                containerClient.CreateItemAsync<QuizEntity>(
                    { id = getCodeFromQuiz quiz
                      data = quiz }
                )
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.ignore
        }

let saveQuiz (tenantName: string) (cosmosClient: CosmosClient) =
    fun quiz ->
        asyncResult {
            let! database = cosmosClient |> ensureBibleQuizDatabase tenantName

            let! containerClient =
                database.Database
                |> ensureQuizContainer
                |> AsyncResult.map (fun container -> container.Container)

            let code = getCodeFromQuiz quiz

            do!
                containerClient.ReplaceItemAsync<QuizEntity>({ id = code; data = quiz }, code)
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.ignore
        }

let getQuiz  (tenantName: string) (cosmosClient: CosmosClient) =
    fun quizCode ->
        asyncResult {
            let! database = cosmosClient |> ensureBibleQuizDatabase tenantName

            let! containerClient =
                database.Database
                |> ensureQuizContainer
                |> AsyncResult.map (fun container -> container.Container)

            let! quiz =
                containerClient.ReadItemAsync<QuizEntity>(quizCode, new PartitionKey(quizCode))
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.map (fun item -> item.Resource.data)

            return quiz
        }

let tryGetQuiz  (tenantName: string) (cosmosClient: CosmosClient) =
    fun quizCode ->
        asyncResult {
            let! database = cosmosClient |> ensureBibleQuizDatabase tenantName

            let! containerClient =
                database.Database
                |> ensureQuizContainer
                |> AsyncResult.map (fun container -> container.Container)

            let! quiz =
                containerClient.ReadItemAsync<QuizEntity>(quizCode, new PartitionKey(quizCode))
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> Async.map (fun result ->
                    match result with
                    | Ok item -> item.Resource.data |> Some |> Ok
                    | Error error ->
                        match error with
                        | DbError.Exception ex ->
                            if ex.InnerException :? CosmosException then
                                let cosmosException = ex.InnerException :?> CosmosException
                                if cosmosException.StatusCode = HttpStatusCode.NotFound then
                                    None |> Ok
                                else
                                    Error error
                            else
                                Error error
                        | _ -> error |> Error)
            return quiz
        }


    
