module FreeMethodist.BibleQuizTracker.Server.Persistence_CosmosDb

open System.IO
open System.Text.Json
open System.Threading
open Azure.Core.Serialization
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.Azure.Cosmos
open Newtonsoft.Json

type CosmosSystemTextJsonSerializer(jsonSerializerOptions: JsonSerializerOptions) =
    inherit CosmosSerializer()
    let systemTextJsonSerializer = JsonObjectSerializer(jsonSerializerOptions)

    override this.FromStream<'T>(stream: Stream) : 'T =
        use useStream = stream
        systemTextJsonSerializer.Deserialize(useStream, typedefof<'T>, CancellationToken.None) :?> 'T

    override this.ToStream<'T>(input: 'T) : Stream =
        let streamPayload = new MemoryStream()
        systemTextJsonSerializer.Serialize(streamPayload, input, typeof<'T>, CancellationToken.None)
        streamPayload.Position <- 0
        streamPayload

let createCosmosClient serializationOptions connectionString =
    new CosmosClient(
        connectionString,
        new CosmosClientOptions(Serializer = CosmosSystemTextJsonSerializer(serializationOptions))
    )

let ensureBibleQuizDatabase tenantName (client: CosmosClient) =
    client.CreateDatabaseIfNotExistsAsync(tenantName)
    |> Async.AwaitTask
    |> Async.catchExceptionsAsErrors DbError.Exception

let ensureQuizContainer (database: Database) =
    database.CreateContainerIfNotExistsAsync("quiz", "/id")
    |> Async.AwaitTask
    |> Async.catchExceptionsAsErrors DbError.Exception
