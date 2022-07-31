namespace FreeMethodist.BibleQuizTracker.Server

open System
open System.Text.Json
open System.Text.Json.Serialization
open Azure.Storage.Blobs
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components.Server.ProtectedBrowserStorage
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.SignalR
open Microsoft.AspNetCore.SignalR.Client
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Bolero
open Bolero.Remoting.Server
open Bolero.Server
open FreeMethodist.BibleQuizTracker
open Bolero.Templating.Server
open Microsoft.Extensions.Logging

type Startup() =

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

        let fsharpJsonOptions =
            JsonSerializerOptions()

        fsharpJsonOptions.Converters.Add(JsonFSharpConverter())

        let getQuiz (provider: IServiceProvider) =
            let localStorage =
                provider.GetRequiredService<ProtectedLocalStorage>()

            let blobServiceClient =
                provider.GetRequiredService<BlobServiceClient>()

            let getFromLocal =
                Persistence.getQuizFromLocalStorage localStorage fsharpJsonOptions

            let getFromBlob =
                Persistence.getQuizFromBlob blobServiceClient fsharpJsonOptions

            Persistence.getQuizFromLocalOrBlob getFromLocal getFromBlob

        let saveQuiz (provider: IServiceProvider) =
            let localStorage =
                provider.GetRequiredService<ProtectedLocalStorage>()

            let blobServiceClient =
                provider.GetRequiredService<BlobServiceClient>()

            let saveToLocal =
                Persistence.saveQuizToLocalStorage localStorage fsharpJsonOptions

            let saveToBlob =
                Persistence.saveQuizToBlob blobServiceClient fsharpJsonOptions

            Persistence.saveQuizToLocalOrBlob saveToLocal saveToBlob

        services
            .AddScoped<GetQuiz>(Func<IServiceProvider, GetQuiz>(getQuiz))
            .AddScoped<SaveQuiz>(Func<IServiceProvider, SaveQuiz>(saveQuiz))
            .AddScoped<SaveNewQuiz>(
                Func<IServiceProvider, SaveNewQuiz> (fun provider ->
                    let blobServiceClient =
                        provider.GetRequiredService<BlobServiceClient>()

                    Persistence.saveNewQuizToBlob blobServiceClient fsharpJsonOptions)
            )
            .AddScoped<TryGetQuiz>(
                Func<IServiceProvider, TryGetQuiz> (fun provider ->
                    let blobServiceClient =
                        provider.GetRequiredService<BlobServiceClient>()

                    let localStorage =
                        provider.GetRequiredService<ProtectedLocalStorage>()

                    let tryGetLocal =
                        Persistence.tryGetQuizFromLocalStorage localStorage fsharpJsonOptions

                    let deserialize (json: string) =
                        JsonSerializer.Deserialize(json, fsharpJsonOptions)

                    let tryGetBlob =
                        Persistence.tryGetQuizFromBlob blobServiceClient deserialize

                    Persistence.tryGetQuizFromLocalOrBlob tryGetLocal tryGetBlob)
            )

            .AddScoped<HubConnection>(
                Func<IServiceProvider, HubConnection> (fun provider ->
                    let configureLogging (logging: ILoggingBuilder) = logging.AddConsole() |> ignore

                    let navigator =
                        provider.GetRequiredService<NavigationManager>()

                    HubConnectionBuilder()
                        .WithUrl($"{navigator.BaseUri}QuizHub")
                        .WithAutomaticReconnect()
                        .ConfigureLogging(configureLogging)
                        .AddJsonProtocol(fun options ->
                            options.PayloadSerializerOptions.Converters.Add(JsonFSharpConverter()))
                        .Build())
            )
        |> ignore

        //So that Discriminated unions can be serialized/deserialized
        services.Configure (fun (options: JsonHubProtocolOptions) ->
            options.PayloadSerializerOptions.Converters.Add(JsonFSharpConverter()))
        |> ignore

        services.AddScoped<BlobServiceClient>(
            Func<IServiceProvider, BlobServiceClient> (fun services ->
                let configuration =
                    services.GetRequiredService<IConfiguration>()

                let connectionString =
                    configuration["BLOBSTORAGE_CONNECTION_STRING"]

                BlobServiceClient(connectionString))
        )
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

                endpoints.MapHub<QuizHub.Hub>(PathString "/quizhub")
                |> ignore

                endpoints.MapFallbackToBolero(Index.page)
                |> ignore)
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
