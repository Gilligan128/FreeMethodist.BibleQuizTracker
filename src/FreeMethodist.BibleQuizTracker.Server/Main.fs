module FreeMethodist.BibleQuizTracker.Server.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Microsoft.AspNetCore.SignalR.Client

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/quiz">] Quiz

/// The Elmish application's model.
type Model =
    { page: Page
      error: string option
      quiz: QuizPage.Model }

and Book =
    { title: string
      author: string
      publishDate: DateTime
      isbn: string }

let initModel =
    { page = Home
      error = None
      quiz = QuizPage.initModel }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | ClearError
    | QuizMessage of QuizPage.Message

let hubConnection =
    HubConnectionBuilder()
        .WithUrl("http://localhost:5000/QuizHub")
        .WithAutomaticReconnect()
        .Build()

let update message model =
    match message with
    | SetPage page -> { model with page = page }, Cmd.none
    | ClearError -> { model with error = None }, Cmd.none
    | QuizMessage quizMsg ->
        let (quizModel, quizCommand) =
            QuizPage.update hubConnection quizMsg model.quiz

        { model with quiz = quizModel }, quizCommand

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
                menuItem model Quiz "Quiz"
            }
        )
        .Body(
            cond model.page
            <| function
                | Home -> homePage model dispatch
                | Quiz -> QuizPage.page model.quiz (fun quizMsg -> dispatch (QuizMessage quizMsg))
        )
        .Error(
            cond model.error
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

let subscription model =
    Cmd.batch [
        Cmd.map Message.QuizMessage (QuizPage.subscribe hubConnection model.quiz)
    ]
    
hubConnection.StartAsync() |> ignore
type MyApp() =
    inherit ProgramComponent<Model, Message>()
                                                                    
    override this.Program =
        Program.mkProgram (fun _ ->
            initModel, Cmd.none) update view
        |> Program.withRouter router
        |> Program.withSubscription subscription
#if DEBUG
        |> Program.withHotReload
#endif
