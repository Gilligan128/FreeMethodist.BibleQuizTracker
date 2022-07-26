module FreeMethodist.BibleQuizTracker.Server.Main

open System
open System.Threading
open Azure.Storage.Blobs
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting.Client
open Bolero.Templating.Client
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.QuizPage
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.SignalR.Client
open Microsoft.FSharp.Control



/// The Elmish application's model.
type Model =
    { page: Page
      Error: string option
      Quiz: QuizPage.Model option }

let initModel =
    { page = Home
      Error = None
      Quiz = None }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | ClearError
    | QuizMessage of QuizPage.Message

let clientStub =
                Unchecked.defaultof<QuizHub.Client>

let runQuizEventOccuredName =
    nameof clientStub.RunQuizEventOccurred

let update
    connectToQuizEvents
    onQuizEvent
    publishQuizEvent
    getQuizAsync
    saveQuizAsync
    (hubConnection: HubConnection)
    (message: Message)
    model
    : Model * Cmd<Message> =
        
    let disconnectFromQuizCmd (quiz: QuizPage.Model) =
            hubConnection.Remove( (runQuizEventOccuredName)) 
            hubConnection.InvokeAsync(
                (nameof
                    Unchecked.defaultof<QuizHub.Hub>
                        .DisconnectFromQuiz),
                quiz.Code,
                CancellationToken.None)
            |> Async.AwaitTask
            |> Async.map (fun _ -> Message.ClearError)
            |> Cmd.OfAsync.result
            
    match message, model.Quiz with
    | SetPage (Page.Quiz quizCode), quizOption ->
        let oldCodeOpt =
            (quizOption |> Option.map (fun q -> q.Code))

        let quizModel, cmd =
            init quizCode oldCodeOpt

        { model with
            page = Quiz quizCode
            Quiz = Some quizModel },
        Cmd.map Message.QuizMessage cmd
    | SetPage page, Some quiz ->
        
        { model with page = page; Quiz = None }, (disconnectFromQuizCmd quiz)
    | SetPage page, None ->  { model with page = page; Quiz = None }, Cmd.none
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

        { newModel with Quiz = Some updatedModel }, Cmd.map QuizMessage quizCommand
    | QuizMessage _, None ->
        { model with Error = Some "A Quiz Message was dispatched, but there is no Quiz Model set" }, Cmd.none

/// Connects the routing system to the Elmish application.
let router =
    Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let homePage model dispatch = Main.Home()
                                  .CreateQuizStart(fun _ -> ())
                                  .JoinQuiz(fun _-> ())
                                  .Elt()

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

let linkToQuizPage (router: Router<Page,Model,Message> ) =
    fun quizCode ->
        router.Link <| Page.Quiz quizCode

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
                    match model.Quiz with
                    | None -> Node.Empty()
                    | Some quizModel -> page (linkToQuizPage router) quizModel (fun quizMsg -> dispatch (QuizMessage quizMsg))
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

    [<Inject>]
    member val BlobServiceClient = Unchecked.defaultof<BlobServiceClient> with get, set

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
          
            fun (handler: RunQuizEvent -> unit) ->
                hubConnection.On<RunQuizEvent>(nameof clientStub.RunQuizEventOccurred, handler)

        let update =
            update connectToQuizEvents onQuizEvent publishQuizEvent this.GetQuizAsync this.SaveQuizAsync hubConnection

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
