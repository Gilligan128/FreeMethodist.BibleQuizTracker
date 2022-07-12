namespace FreeMethodist.BibleQuizTracker.Server

open System.Text.Json.Serialization
open FreeMethodist.BibleQuizTracker.Server.Pipeline
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Builder
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
        services.AddSingleton<SaveTeamQuiz>(Persistence.saveQuiz)
                .AddSingleton<GetTeamQuiz>(Persistence.getQuiz) |> ignore 
        
        //So that Discriminated unions can bd serialized/deserialized
        services.Configure (fun (options: JsonHubProtocolOptions) ->
            options.PayloadSerializerOptions.Converters.Add(JsonFSharpConverter())
            |> ignore)
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
        let loggingConfig (logging:ILoggingBuilder) =
            logging.AddFilter("Microsoft.AspNetCore.SignalR", LogLevel.Debug)
                   .AddFilter("Microsoft.AspNetCore.Http.Connections", LogLevel.Debug) |> ignore
            
        WebHost
            .CreateDefaultBuilder(args)
            .UseStaticWebAssets()
            .ConfigureLogging(loggingConfig)
            .UseStartup<Startup>()
            .Build()
            .Run()

        0
