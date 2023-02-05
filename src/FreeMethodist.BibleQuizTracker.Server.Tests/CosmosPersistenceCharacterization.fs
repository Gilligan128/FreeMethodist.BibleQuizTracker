module FreeMethodist.BibleQuizTracker.Server.Tests.CosmosPersistenceCharacterization

open System
open System.Net
open System.Text.Json
open System.Text.Json.Serialization
open Azure
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Persistence_CosmosDb
open Microsoft.Azure.Cosmos
open Microsoft.Extensions.Configuration
open Xunit
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open Xunit.Sdk

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

    use cosmosClient = new CosmosClient(connectionString, new CosmosClientOptions(Serializer = CosmosSystemTextJsonSerializer(fsharpJsonOptions)))

    do
        cosmosClient.CreateDatabaseIfNotExistsAsync($"{Environment.MachineName}test")
        |> Async.AwaitTask
        |> Async.bind (fun db ->
            db.Database.CreateContainerIfNotExistsAsync("quiz", "/TournamentInfo/Link")
            |> Async.AwaitTask)
        |> Async.bind (fun container -> container.Container.DeleteContainerAsync() |> Async.AwaitTask)
        |> Async.Ignore
        |> Async.RunSynchronously
        
    let saveQuiz =
        QuizState_Persistence_CosmosDb.saveNewQuiz cosmosClient $"{Environment.MachineName}test"

    let quiz = Running RunningQuiz.newTeamQuiz

    let result = saveQuiz quiz |> Async.RunSynchronously
    
    Assert.Equal(Ok(), result)

