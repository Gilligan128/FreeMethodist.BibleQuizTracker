namespace FreeMethodist.BibleQuizTracker.Server

open System
open System.Text.Json
open System.Text.Json.Serialization
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Components.Server.ProtectedBrowserStorage
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.SignalR
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
        let fsharpJsonOptions = JsonSerializerOptions()
        fsharpJsonOptions.Converters.Add(JsonFSharpConverter())
        
        //since we are storing in-memory, we need to ensure this is a singleton across Blazor Circuits
        let getTeam (c:IServiceProvider) =
                let localStorage = c.GetRequiredService<ProtectedLocalStorage>()
                Persistence.getQuizFromLocalStorage localStorage fsharpJsonOptions
        let saveTeam (provider: IServiceProvider) =
            let localStorage = provider.GetRequiredService<ProtectedLocalStorage>()
            Persistence.saveQuizToLocalStorage localStorage fsharpJsonOptions
        services
            .AddTransient<GetTeamQuizAsync>(Func<IServiceProvider, GetTeamQuizAsync>(getTeam))
            .AddTransient<SaveTeamQuizAsync>(Func<IServiceProvider, SaveTeamQuizAsync>(saveTeam))
        |> ignore

        //So that Discriminated unions can bd serialized/deserialized
        services.Configure (fun (options: JsonHubProtocolOptions) ->
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
