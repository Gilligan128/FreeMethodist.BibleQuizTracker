module FreeMethodist.BibleQuizTracker.Server.Tests.PersistenceCharacterization

open System.Text.Json
open System.Text.Json.Serialization
open Azure
open Azure.Storage.Blobs
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.Extensions.Configuration
open Xunit
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

let createJsonOptions =
    fun () ->
        let options = JsonSerializerOptions()
        options.Converters.Add(JsonFSharpConverter())
        options


[<Fact(Skip="Characterization Test")>]
let ``Save Blob`` () =
   
    let fsharpJsonOptions = createJsonOptions ()

    let configuration =
        ConfigurationBuilder()
            .AddUserSecrets<Startup>()
            .Build()

    let blobConnectionString =
        configuration["BLOBSTORAGE_CONNECTION_STRING"]

    let blobServiceClient =
        BlobServiceClient(blobConnectionString)
    
    try
        blobServiceClient.DeleteBlobContainer("quizzes")  |> ignore
    with :? RequestFailedException as ex when ex.Status = 404 -> () 
    
    let saveQuiz =
        Persistence.saveQuizToBlob blobServiceClient fsharpJsonOptions

    let result = saveQuiz (Running RunningTeamQuiz.identity) |> Async.RunSynchronously
    
    Assert.Equal(Ok (), result)
