module FreeMethodist.BibleQuizTracker.Server.Tests.WtfGetQuiz

open System.Text.Json
open System.Text.Json.Serialization
open Azure.Storage.Blobs
open FreeMethodist.BibleQuizTracker.Server
open Microsoft.Extensions.Configuration
open Xunit

[<Fact>]
let ``Integration infrastructure reference`` () =
    //just some things we will need for integration testing later.
    let fsharpJsonOptions =
            JsonSerializerOptions()

    fsharpJsonOptions.Converters.Add(JsonFSharpConverter())
    
    let configuration =
        ConfigurationBuilder().AddUserSecrets<Startup>().Build()
    let blobConnectionString = configuration["BLOBSTORAGE_CONNECTION_STRING"]
    
    Assert.True(true)
