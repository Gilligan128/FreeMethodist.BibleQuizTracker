module FreeMethodist.BibleQuizTracker.Server.Tests.CosmosPersistenceCharacterization

open System
open System.Net
open System.Text.Json
open System.Text.Json.Serialization
open Azure
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.QuizState_Persistence_CosmosDb
open Microsoft.Azure.Cosmos
open Microsoft.Extensions.Configuration
open Xunit
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open Xunit.Sdk
open Persistence_CosmosDb

let createJsonOptions =
    fun () ->
        let options = JsonSerializerOptions()
        options.Converters.Add(JsonFSharpConverter())
        options


let deleteContainer (container: Container) =
    container.DeleteContainerAsync() |> Async.AwaitTask

type CosmosDbPersistenceCharacterization() =
    let fsharpJsonOptions = createJsonOptions ()

    let configuration =
        ConfigurationBuilder()
            .AddEnvironmentVariables()
            .AddUserSecrets<Startup>()
            .Build()

    let connectionString = configuration["COSMOSDB_CONNECTION_STRING"]
    let cosmosDbClient = createCosmosClient fsharpJsonOptions connectionString
    let tenant = $"{Environment.MachineName}test"

    let containerResult =
        cosmosDbClient
        |> ensureBibleQuizDatabase tenant
        |> AsyncResult.map (fun db -> db.Database)
        |> AsyncResult.bind ensureQuizContainer
        |> AsyncResult.map (fun container -> container.Container)

    do
        containerResult
        |> AsyncResult.bind (deleteContainer >> AsyncResult.ofAsync)
        |> Async.RunSynchronously
        |> ignore


    interface IDisposable with
        member this.Dispose() = cosmosDbClient.Dispose()

    [<Fact(Skip = "Characterization test")>]
    member _.``Save quiz``() =

        let saveQuiz = cosmosDbClient |> saveNewQuiz tenant

        let quiz = Running RunningQuiz.newTeamQuiz

        let result = saveQuiz quiz |> Async.RunSynchronously

        Assert.Equal(Ok(), result) |> ignore

        let items =
            containerResult
            |> AsyncResult.map (fun container ->
                container.GetItemLinqQueryable<QuizEntity>(allowSynchronousQueryExecution = true))
            |> Async.RunSynchronously
            |> function
                | Ok items -> items
                | Error _ -> failwith "Could not get items"
            |> Seq.toList

        Assert.NotEmpty(items)


    [<Fact(Skip = "Characterization Test")>]
    member _.``Try get quiz when not found``() =
        let result =
            (cosmosDbClient |> tryGetQuiz tenant) "not_fount" |> Async.RunSynchronously

        Assert.Equal(Ok None, result)

    [<Fact(Skip = "Characterization Test")>]
    member _.``list quizzes``() =
        let quiz = Running RunningQuiz.newTeamQuiz
        let saveQuiz = cosmosDbClient |> saveNewQuiz tenant

        do saveQuiz quiz |> Async.RunSynchronously |> ignore

        let input = { Status = QuizStatusFilter.Running }
        let result = getQuizzes tenant cosmosDbClient input |> Async.RunSynchronously

        Assert.Equal(Ok 1, result |> Result.map Seq.length)
