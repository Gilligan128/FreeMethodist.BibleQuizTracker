module FreeMethodist.BibleQuizTracker.Server.Tests.PersistenceCharacterization

open System
open System.Text.Json
open System.Text.Json.Serialization
open Azure
open Azure.Storage.Blobs
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.QuizState_Versioning
open FreeMethodist.BibleQuizTracker.Server.Tournament
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.Extensions.Configuration
open Xunit
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

let createJsonOptions =
    fun () ->
        let options = JsonSerializerOptions()
        options.Converters.Add(JsonFSharpConverter())
        options

let ignore404 defaultValue io =
    try
        io ()
    with :? RequestFailedException as ex when ex.Status = 404 ->
        defaultValue


[<Fact(Skip = "Characterization Test")>]
let ``Save Blob`` () =

    let fsharpJsonOptions = createJsonOptions ()

    let configuration = ConfigurationBuilder().AddUserSecrets<Startup>().Build()

    let blobConnectionString = configuration["BLOBSTORAGE_CONNECTION_STRING"]

    let blobServiceClient = BlobServiceClient(blobConnectionString)

    ignore404 () (fun () -> blobServiceClient.DeleteBlobContainer("quizzes") |> ignore)

    let saveQuiz =
        Persistence.saveQuizToBlob blobServiceClient fsharpJsonOptions $"{Environment.MachineName}test"

    let result = saveQuiz (Running RunningQuiz.newTeamQuiz) |> Async.RunSynchronously

    Assert.Equal(Ok(), result)

[<Fact(Skip = "Characterization Test")>]
let ``Get Completed`` () =

    let fsharpJsonOptions = createJsonOptions ()

    let configuration = ConfigurationBuilder().AddUserSecrets<Startup>().Build()

    let blobConnectionString = configuration["BLOBSTORAGE_CONNECTION_STRING"]

    let blobServiceClient = BlobServiceClient(blobConnectionString)

    let containerClient = blobServiceClient.GetBlobContainerClient("quizzes")

    ignore404 Unchecked.defaultof<Response> (fun () ->
        containerClient.CreateIfNotExists() |> ignore
        containerClient.DeleteBlob("completed"))
    |> ignore

    let saveQuiz =
        Persistence.saveQuizToBlob blobServiceClient fsharpJsonOptions $"{Environment.MachineName}test"
    
    asyncResult {
        let completedQuiz =
            Completed
                { Code = "completed"
                  TournamentInfo = TournamentInfo.empty |> Info
                  CompletedQuestions = []
                  CompetitionStyle = CompletedCompetitionStyle.Individual [] }

        do! saveQuiz completedQuiz

        let! result = Persistence.getRecentCompletedQuizzes blobServiceClient $"{Environment.MachineName}test"

        Assert.NotEmpty(result)
    }
