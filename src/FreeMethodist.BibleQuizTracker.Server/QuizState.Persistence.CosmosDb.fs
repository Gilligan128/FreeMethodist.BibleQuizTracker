module FreeMethodist.BibleQuizTracker.Server.QuizState_Persistence_CosmosDb

open System.Text.Json
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.Azure.Cosmos

type QuizEntity = { id : string; data : Quiz }

let private getCodeFromQuiz quiz =
    match quiz with
    | Running runningTeamQuiz -> runningTeamQuiz.Code
    | Completed completedTeamQuiz -> completedTeamQuiz.Code
    | Official officialTeamQuiz -> officialTeamQuiz.Code

let private getBlobName quizCode = $"quiz-{quizCode}"

let saveNewQuiz (cosmosClient: CosmosClient) (tenantName: string)  =
    fun quiz ->
        asyncResult {
            let! database =
                cosmosClient.CreateDatabaseIfNotExistsAsync(tenantName)
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception

            let! containerClient =
                database.Database.CreateContainerIfNotExistsAsync("quiz", "/TournamentInfo/Link")
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.map (fun container -> container.Container)

            do!
                containerClient.CreateItemAsync<QuizEntity>({ id = getCodeFromQuiz quiz; data = quiz})
                |> Async.AwaitTask
                |> Async.catchExceptionsAsErrors DbError.Exception
                |> AsyncResult.ignore
        }

