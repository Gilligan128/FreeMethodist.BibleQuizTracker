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

let ignore404 defaultValue io =
    try
        io ()
    with :? CosmosException as ex when ex.StatusCode = HttpStatusCode.NotFound ->
        defaultValue


[<Fact(Skip= "Characterization Test")>]
let ``Save Blob`` () =

    let fsharpJsonOptions = createJsonOptions ()

    let configuration = ConfigurationBuilder().AddUserSecrets<Startup>().Build()

    let connectionString = configuration["COSMOSDB_CONNECTION_STRING"]

    use cosmosClient = createCosmosClient fsharpJsonOptions connectionString
    
    let containerResponseResultAsync = 
        cosmosClient
        |> ensureBibleQuizDatabase $"{Environment.MachineName}test"
        |> AsyncResult.map (fun db -> db.Database)
        |> AsyncResult.bind ensureQuizContainer
        |> AsyncResult.map (fun container -> container.Container)
    do
        containerResponseResultAsync
        |> AsyncResult.bind (fun container -> container.DeleteContainerAsync() |> Async.AwaitTask |> AsyncResult.ofAsync)
        |> Async.Ignore
        |> Async.RunSynchronously
        
    let saveQuiz =
        QuizState_Persistence_CosmosDb.saveNewQuiz cosmosClient $"{Environment.MachineName}test"

    let quiz = Running RunningQuiz.newTeamQuiz

    let result = saveQuiz quiz |> Async.RunSynchronously
    
    Assert.Equal(Ok(), result) |> ignore
    
    let items = containerResponseResultAsync
                |> AsyncResult.map (fun container -> container.GetItemLinqQueryable<QuizEntity>(allowSynchronousQueryExecution = true))
                |> Async.RunSynchronously
                |> function Ok items -> items | Error _ -> failwith "Could not get items"
                |> Seq.toList
    Assert.NotEmpty(items)
