module FreeMethodist.BibleQuizTracker.Server.Tests.WtfGetQuiz

open System.Text.Json
open System.Text.Json.Serialization
open Azure.Storage.Blobs
open FreeMethodist.BibleQuizTracker.Server
open Microsoft.Extensions.Configuration
open Xunit

[<Fact>]
let ``GetQuiz 404s`` () =
    let fsharpJsonOptions =
            JsonSerializerOptions()

    fsharpJsonOptions.Converters.Add(JsonFSharpConverter())
    
    let configuration =
        ConfigurationBuilder().AddUserSecrets<Startup>().Build()
    let blobConnectionString = configuration["BLOBSTORAGE_CONNECTION_STRING"]
    let blobServiceClient = BlobServiceClient(blobConnectionString)
    
    let blob = Persistence.getQuizFromBlob blobServiceClient fsharpJsonOptions "adf;asdfopiauj"
    let result = blob |> Async.RunSynchronously
    Assert.True(result |> Result.isError)
