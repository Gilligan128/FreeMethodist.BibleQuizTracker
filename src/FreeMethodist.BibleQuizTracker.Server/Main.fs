module FreeMethodist.BibleQuizTracker.Server.Main

open System
open System.Text.Json.Serialization
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open FreeMethodist.BibleQuizTracker.Server.QuizPage
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.SignalR.Client
open Microsoft.Extensions.DependencyInjection
open Microsoft.FSharp.Control

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/quiz">] Quiz

/// The Elmish application's model.
type Model =
    { page: Page
      Error: string option
      quiz: QuizPage.Model }

and Book =
    { title: string
      author: string
      publishDate: DateTime
      isbn: string }

let initModel initQuizModel =
    { page = Home
      Error = None
      quiz = initQuizModel }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | ClearError
    | QuizMessage of QuizPage.Message



let update hubConnection getQuiz saveQuiz (message: Message) model : Model * Cmd<Message> =
    match message with
    | SetPage page -> { model with page = page }, Cmd.none
    | ClearError -> { model with Error = None }, Cmd.none
    | QuizMessage quizMsg ->
        let (quizModel, quizCommand, externalMessage) =
            QuizPage.update hubConnection getQuiz saveQuiz quizMsg model.quiz

        let newModel =
            match externalMessage with
            | None -> { model with Error = None }
            | Some message ->
                match message with
                | Error er -> { model with Error = Some er }

        { newModel with quiz = quizModel }, Cmd.map QuizMessage quizCommand

/// Connects the routing system to the Elmish application.
let router =
    Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let homePage model dispatch = Main.Home().Elt()

let menuItem (model: Model) (page: Page) (text: string) =
    Main
        .MenuItem()
        .Active(
            if model.page = page then
                "is-active"
            else
                ""
        )
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(
            concat {
                menuItem model Home "Home"
                menuItem model Quiz "Quiz Example"
            }
        )
        .Body(
            cond model.page
            <| function
                | Home -> homePage model dispatch
                | Quiz -> QuizPage.page model.quiz (fun quizMsg -> dispatch (QuizMessage quizMsg))
        )
        .Error(
            cond model.Error
            <| function
                | None -> empty ()
                | Some err ->
                    Main
                        .ErrorNotification()
                        .Text(err)
                        .Hide(fun _ -> dispatch ClearError)
                        .Elt()
        )
        .Elt()

let subscription hubConnection model =
    Cmd.batch [ Cmd.map Message.QuizMessage (QuizPage.subscribe hubConnection model.quiz) ]


type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val Navigator = Unchecked.defaultof<NavigationManager> with get, set

    [<Inject>]
    member val SaveQuiz = Unchecked.defaultof<SaveTeamQuiz> with get, set

    [<Inject>]
    member val GetQuiz = Unchecked.defaultof<GetTeamQuiz> with get, set

    override this.Program =
        let hubConnection =
            HubConnectionBuilder()
                .WithUrl($"{this.Navigator.BaseUri}QuizHub")
                .WithAutomaticReconnect()
                .AddJsonProtocol(fun options -> options.PayloadSerializerOptions.Converters.Add(JsonFSharpConverter()))
                .Build()

        let update =
            update hubConnection this.GetQuiz this.SaveQuiz

        let subscription =
            subscription hubConnection

        Program.mkProgram
            (fun _ ->
                hubConnection.StartAsync() |> ignore

                let initQuizModel =
                    QuizPage.init this.GetQuiz

                (initModel initQuizModel), Cmd.none)
            update
            view
        |> Program.withRouter router
        |> Program.withSubscription subscription
#if DEBUG
        |> Program.withHotReload
#endif
