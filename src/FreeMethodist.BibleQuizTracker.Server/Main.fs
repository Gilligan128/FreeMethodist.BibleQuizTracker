module FreeMethodist.BibleQuizTracker.Server.Main

open System
open System.Threading
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting.Client
open Bolero.Templating.Client
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.QuizPage
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.SignalR.Client
open Microsoft.FSharp.Control

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/quiz">] Quiz of quizCode: string

/// The Elmish application's model.
type Model =
    { page: Page
      Error: string option
      quiz: QuizPage.Model option }

and Book =
    { title: string
      author: string
      publishDate: DateTime
      isbn: string }

let initModel =
    { page = Home
      Error = None
      quiz = None }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | ClearError
    | QuizMessage of QuizPage.Message


let update
    connectToQuizEvents
    onQuizEvent
    publishQuizEvent
    getQuizAsync
    saveQuizAsync
    (message: Message)
    model
    : Model * Cmd<Message> =
    match message, model.quiz with
    | SetPage (Page.Quiz quizCode), quizOption ->
        let oldCodeOpt =
            (quizOption |> Option.map (fun q -> q.Code))

        let quizModel, cmd =
            init quizCode oldCodeOpt

        { model with
            page = Quiz quizCode
            quiz = Some quizModel },
        Cmd.map Message.QuizMessage cmd
    | SetPage page, _ -> { model with page = page; quiz = None }, Cmd.none
    | ClearError, _ -> { model with Error = None }, Cmd.none
    | QuizMessage quizMsg, Some quizModel ->
        let (updatedModel, quizCommand, externalMessage) =
            update connectToQuizEvents onQuizEvent publishQuizEvent getQuizAsync saveQuizAsync quizMsg quizModel

        let newModel =
            match externalMessage with
            | None -> { model with Error = None }
            | Some message ->
                match message with
                | Error er -> { model with Error = Some er }

        { newModel with quiz = Some updatedModel }, Cmd.map QuizMessage quizCommand
    | QuizMessage _, None ->
        { model with Error = Some "A Quiz Message was dispatched, but there is no Quiz Model set" }, Cmd.none

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
                menuItem model (Quiz "Example") "Quiz Example"
            }
        )
        .Body(
            cond model.page
            <| function
                | Home -> homePage model dispatch
                | Quiz code ->
                    match model.quiz with
                    | None -> Node.Empty()
                    | Some quizModel -> QuizPage.page quizModel (fun quizMsg -> dispatch (QuizMessage quizMsg))
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

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val Navigator = Unchecked.defaultof<NavigationManager> with get, set

    [<Inject>]
    member val GetQuizAsync = Unchecked.defaultof<GetTeamQuizAsync> with get, set

    [<Inject>]
    member val SaveQuizAsync = Unchecked.defaultof<SaveTeamQuizAsync> with get, set

    [<Inject>]
    member val HubConnection = Unchecked.defaultof<HubConnection> with get, set

    override this.Program =
        let hubConnection = this.HubConnection

        let connectToQuizEvents quizCode previousQuizCode =
            hubConnection.InvokeAsync("ConnectToQuiz", quizCode, previousQuizCode, CancellationToken.None)
            |> Async.AwaitTask

        let publishQuizEvent =
            fun methodName quiz event ->
                hubConnection.InvokeAsync(methodName, quiz, event, CancellationToken.None)
                |> Async.AwaitTask

        let onQuizEvent =
            let clientStub =
                Unchecked.defaultof<QuizHub.Client>

            fun (handler: RunQuizEvent -> unit) ->
                hubConnection.On<RunQuizEvent>(nameof clientStub.RunQuizEventOccurred, handler)

        let update =
            update connectToQuizEvents onQuizEvent publishQuizEvent this.GetQuizAsync this.SaveQuizAsync

        Program.mkProgram
            (fun _ ->
                hubConnection.StartAsync() |> ignore
                initModel, Cmd.none)
            update
            view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
