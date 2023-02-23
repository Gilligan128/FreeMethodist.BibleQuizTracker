namespace FreeMethodist.BibleQuizTracker.Server

open System
open System.Text.Json
open System.Text.Json.Serialization
open Azure.Storage.Blobs
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.QuizState_Persistence_CosmosDb
open FreeMethodist.BibleQuizTracker.Server.QuizState_Versioning
open FreeMethodist.BibleQuizTracker.Server.QuizQueries
open FreeMethodist.BibleQuizTracker.Server.Serialization
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components.Server.ProtectedBrowserStorage
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.SignalR
open Microsoft.AspNetCore.SignalR.Client
open Microsoft.Azure.Cosmos
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Bolero.Server
open Bolero.Templating.Server
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging


type Startup(configuration: IConfiguration) =

    let getTenantName machineName (environment: IHostEnvironment) =
        if environment.EnvironmentName = Environments.Development then
            machineName
        else
            environment.EnvironmentName

    let resolveTenantName (provider: IServiceProvider) =
        provider.GetRequiredService<IHostEnvironment>()
        |> getTenantName Environment.MachineName


    // This method gets called by the runtime. Use this method to add services to the container.
    // For more information on how to configure your application, visit https://go.microsoft.com/fwlink/?LinkID=398940
    member this.ConfigureServices(services: IServiceCollection) =
        services.AddMvc() |> ignore
        services.AddServerSideBlazor() |> ignore

        services
            .AddAuthorization()
            .AddAuthentication(CookieAuthenticationDefaults.AuthenticationScheme)
            .AddCookie()
            .Services.AddBoleroHost(server = true)
#if DEBUG
            .AddHotReload(
                templateDir = __SOURCE_DIRECTORY__
            )
#endif
        |> ignore

        let fsharpJsonOptions = FSharpSerializer.fSharpOptions

        let deserialize (json: string) =
            JsonSerializer.Deserialize(json, fsharpJsonOptions)
            |> QuizVersioning.applyBackwardsCompatibility

        services
            .AddScoped<CosmosClient>(
                Func<IServiceProvider, CosmosClient>(fun services ->
                    Persistence_CosmosDb.createCosmosClient
                        fsharpJsonOptions
                        configuration["COSMOSDB_CONNECTION_STRING"])
            )
            .AddScoped<GetQuiz>(
                Func<IServiceProvider, GetQuiz>(fun provider ->
                    let localStorage = provider.GetRequiredService<ProtectedLocalStorage>()
                    let cosmosClient = provider.GetRequiredService<CosmosClient>()
                    let getFromLocal = Persistence.getQuizFromLocalStorage localStorage deserialize
                    let tenantName = resolveTenantName provider
                    let getFromBlob = QuizState_Persistence_CosmosDb.getQuiz tenantName cosmosClient

                    Persistence.getQuizFromLocalOrDb getFromLocal getFromBlob)
            )
            .AddScoped<SaveQuiz>(
                Func<IServiceProvider, SaveQuiz>(fun provider ->
                    let localStorage = provider.GetRequiredService<ProtectedLocalStorage>()
                    let cosmosClient = provider.GetRequiredService<CosmosClient>()
                    let saveToLocal = Persistence.saveQuizToLocalStorage localStorage fsharpJsonOptions
                    let tenantName = resolveTenantName provider
                    let saveToBlob = QuizState_Persistence_CosmosDb.saveQuiz tenantName cosmosClient
                    Persistence.saveQuizToLocalOrDb saveToLocal saveToBlob)
            )
            .AddScoped<SaveNewQuiz>(
                Func<IServiceProvider, SaveNewQuiz>(fun provider ->
                    let tenantName = resolveTenantName provider
                    let cosmosClient = provider.GetRequiredService<CosmosClient>()

                    QuizState_Persistence_CosmosDb.saveNewQuiz tenantName cosmosClient
                    |> SaveNewQuiz)
            )
            .AddScoped<TryGetQuiz>(
                Func<IServiceProvider, TryGetQuiz>(fun provider ->
                    let localStorage = provider.GetRequiredService<ProtectedLocalStorage>()
                    let cosmosClient = provider.GetRequiredService<CosmosClient>()
                    let tryGetLocal = Persistence.tryGetQuizFromLocalStorage localStorage deserialize
                    let tenantName = resolveTenantName provider
                    let tryGetDb = QuizState_Persistence_CosmosDb.tryGetQuiz tenantName cosmosClient
                    Persistence.tryGetQuizFromLocalOrBlob tryGetLocal tryGetDb)
            )
            .AddScoped<HubConnection>(
                Func<IServiceProvider, HubConnection>(fun provider ->
                    let configureLogging (logging: ILoggingBuilder) = logging.AddConsole() |> ignore

                    let navigator = provider.GetRequiredService<NavigationManager>()

                    HubConnectionBuilder()
                        .WithUrl($"{navigator.BaseUri}QuizHub")
                        .WithAutomaticReconnect()
                        .ConfigureLogging(configureLogging)
                        .AddJsonProtocol(fun options ->
                            options.PayloadSerializerOptions.Converters.Add(JsonFSharpConverter()))
                        .Build())
            )
            .AddScoped<GetRecentCompletedQuizzes>(
                Func<IServiceProvider, GetRecentCompletedQuizzes>(fun provider ->
                    let tenantName = resolveTenantName provider
                    let cosmosClient = provider.GetRequiredService<CosmosClient>()

                    fun () ->
                        { Status = QuizStatusFilter.Completed }
                        |> getQuizzes tenantName cosmosClient
                        |> AsyncResult.map (fun quizzes -> quizzes |> Seq.map Quiz.getCode |> Seq.toList))
            )
            .AddScoped<QuizStatusFilter -> AsyncResult<ListQuizItem list, DbError>>(
                Func<IServiceProvider, QuizStatusFilter -> AsyncResult<ListQuizItem list, DbError>>(fun services ->
                    let tenantName = resolveTenantName services
                    let cosmosClient = services.GetRequiredService<CosmosClient>()

                    fun status ->
                        { Status = status }
                        |> getQuizzes tenantName cosmosClient
                        |> AsyncResult.map (fun quizzes ->
                            quizzes
                            |> Seq.map (fun quiz ->
                                { Code = quiz |> Quiz.getCode
                                  State = ListQuizState.Running
                                  Tournament = None
                                  Round = None
                                  Room = None })
                            |> Seq.toList))
            )
            .Configure(fun (options: JsonHubProtocolOptions) -> //So that Discriminated unions can be serialized/deserialized
                options.PayloadSerializerOptions.Converters.Add(JsonFSharpConverter()))
        |> ignore

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
        app
            .UseAuthentication()
            .UseStaticFiles()
            .UseRouting()
            .UseBlazorFrameworkFiles()
            .UseEndpoints(fun endpoints ->
#if DEBUG
                endpoints.UseHotReload()
#endif
                endpoints.MapBlazorHub() |> ignore

                endpoints.MapHub<QuizHub.Hub>(PathString "/quizhub") |> ignore

                endpoints.MapFallbackToBolero(Index.page) |> ignore)

        |> ignore

module Program =

    [<EntryPoint>]
    let main args =
        let loggingConfig (logging: ILoggingBuilder) =
            logging
                .AddFilter("Microsoft.AspNetCore.SignalR", LogLevel.Debug)
                .AddFilter("Microsoft.AspNetCore.Http.Connections", LogLevel.Debug)
            |> ignore

        WebHost
            .CreateDefaultBuilder(args)
            .UseStaticWebAssets()
            .ConfigureLogging(loggingConfig)
            .UseStartup<Startup>()
            .Build()
            .Run()

        0
