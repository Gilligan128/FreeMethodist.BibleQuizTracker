module FreeMethodist.BibleQuizTracker.Server.Main

open System.Threading
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting.Client
open Bolero.Templating.Client
open FreeMethodist.BibleQuizTracker.Server.Capabilities.Capabilities
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.CreateQuizForm
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.QuizPage
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.SignalR.Client
open Microsoft.FSharp.Control



/// The Elmish application's model.
type Model =
    { page: Page
      Error: string option
      Quiz: QuizPage.Model option
      QuizCode: QuizCode option
      CreateQuizForm: CreateQuizForm.CreateQuizForm.Model }

let initModel =
    { page = Home
      Error = None
      Quiz = None
      QuizCode = None
      CreateQuizForm = CreateQuizForm.CreateQuizForm.Model.Inert }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | ClearError
    | QuizMessage of QuizPage.Message
    | SetQuizCode of string
    | JoinQuiz
    | CreateQuiz of CreateQuizForm.Message
    | LiveScoreMessage of LiveScorePage.Message

let clientStub =
    Unchecked.defaultof<QuizHub.Client>

let runQuizEventOccuredName =
    nameof clientStub.RunQuizEventOccurred

let getCodeFromModel model =
    match model with
    | NotYetStarted (code, _) -> code
    | Loading (code, _) -> code
    | Loaded loaded -> loaded.Code

let previousQuizCode page =
    match page with
    | Page.QuizRun quizCode
    | Page.QuizLiveScore (quizCode, _)
    | Page.QuizSpectate quizCode -> quizCode |> Some
    | Page.Home -> None

let disconnectFromHub (hubConnection: HubConnection) quizCode =
    hubConnection.Remove((runQuizEventOccuredName))

    hubConnection.InvokeAsync(
        (nameof
            Unchecked.defaultof<QuizHub.Hub>
                .DisconnectFromQuiz),
        quizCode,
        CancellationToken.None
    )
    |> Async.awaitPlainTask

let disconnectFromQuizCmd (hubConnection: HubConnection) quizCode =
    fun (_ : Dispatch<Message>) -> disconnectFromHub hubConnection quizCode |> Async.StartImmediate
    |> Cmd.ofSub

let disconnectCmdForPreviousModel disconnectCommand (previousPage: Page) =
    previousPage
    |> previousQuizCode
    |> Option.map disconnectCommand
    |> Option.defaultValue Cmd.none

let update
    connectToQuizEvents
    onQuizEvent
    publishQuizEvent
    getQuizAsync
    saveNewQuiz
    tryGetQuiz
    spectateQuiz
    (hubConnection: HubConnection)
    navigate
    capabilitiesProvider
    (message: Message)
    model
    : Model * Cmd<Message> =

    let navigate = navigate model.page

    let disconnectPreviousPage page =
        disconnectCmdForPreviousModel (disconnectFromQuizCmd hubConnection) page

    let initializeQuiz page user quizCode quizOption =
        let oldCodeOpt =
            (quizOption
             |> Option.bind (fun q ->
                 match q with
                 | NotYetStarted _ -> None
                 | Loading _ -> None
                 | Loaded loaded -> Some loaded.Code))

        let quizModel, cmd =
            init user quizCode oldCodeOpt

        { model with
            page = page
            Quiz = Some quizModel },
        Cmd.map Message.QuizMessage cmd

    let connectToQuizFactory = //to avoid being locked into one message type
        fun () -> (Common_Page.connectAndHandleQuizEvents connectToQuizEvents onQuizEvent)

    match message with
    | SetPage (Page.QuizRun quizCode) ->
        let disconnectCmd =
            disconnectPreviousPage model.page

        let model, initCmd =
            initializeQuiz (QuizRun quizCode) Scorekeeper quizCode None

        model, Cmd.batch [ disconnectCmd; initCmd ]
    | SetPage (QuizSpectate quizCode) ->
        let disconnectCmd =
            disconnectPreviousPage model.page

        let model, initCmd =
            initializeQuiz (QuizSpectate quizCode) Spectator quizCode None

        { model with page = Page.QuizRun quizCode }, Cmd.batch [ disconnectCmd; initCmd ]
    | SetPage (QuizLiveScore (quizCode, pageModel)) ->
        let disconnectCmd =
            disconnectPreviousPage model.page

        let scoreModel, initCmd =
            LiveScorePage.init quizCode

        { model with page = QuizLiveScore(quizCode, { Model = scoreModel }) },
        Cmd.batch [ disconnectCmd
                    initCmd |> Cmd.map LiveScoreMessage ]
    | SetPage page ->
        let disconnectCmd =
            disconnectPreviousPage model.page

        { model with page = page; Quiz = None }, disconnectCmd
    | ClearError -> { model with Error = None }, Cmd.none
    | QuizMessage quizMsg ->
        match model.Quiz with
        | Some quizModel ->
            let (updatedModel, quizCommand, externalMessage) =
                update
                    (connectToQuizFactory ())
                    publishQuizEvent
                    getQuizAsync
                    tryGetQuiz
                    navigate
                    capabilitiesProvider
                    quizMsg
                    quizModel

            let newModel =
                match externalMessage with
                | None -> { model with Error = None }
                | Some message ->
                    match message with
                    | Error er -> { model with Error = Some er }

            { newModel with Quiz = Some updatedModel }, Cmd.map QuizMessage quizCommand
        | None -> { model with Error = Some "A Quiz Message was dispatched, but there is no Quiz Model set" }, Cmd.none
    | SetQuizCode code -> { model with QuizCode = Some code }, Cmd.none
    | JoinQuiz -> { model with Error = Some "Join Quiz not yet implemented" }, Cmd.none
    | CreateQuiz message ->
        let createQuizModel, cmd =
            CreateQuizForm.update
                HumanReadableIds.HumanReadableIds.generateCode
                saveNewQuiz
                spectateQuiz
                message
                model.CreateQuizForm

        { model with CreateQuizForm = createQuizModel }, cmd |> Cmd.map Message.CreateQuiz
    | LiveScoreMessage message ->
        let updateLiveScore message =
            match model.page with
            | QuizLiveScore (code, liveScoreModel) ->

                let newScoreModel, cmd =
                    LiveScorePage.update (connectToQuizFactory ()) tryGetQuiz liveScoreModel.Model message

                let newModel =
                    { model with page = QuizLiveScore(code, { Model = newScoreModel }) }

                let newCmd = cmd |> Cmd.map LiveScoreMessage
                newModel, newCmd
            | _ -> model, Cmd.none

        updateLiveScore message


/// Connects the routing system to the Elmish application.
let defaultModel =
    function
    | QuizRun _ -> ()
    | QuizSpectate _ -> ()
    | Home -> ()
    | QuizLiveScore (quizCode, pageModel) ->
        Router.definePageModel
            pageModel
            { Code = ""
              Scores = Deferred.NotYetStarted }

let router =
    Router.inferWithModel SetPage (fun model -> model.page) defaultModel

type Main = Template<"wwwroot/main.html">

let homePage model dispatch =
    Main
        .Home()
        .CreateQuizStart(fun _ ->
            dispatch << Message.CreateQuiz
            <| CreateQuizForm.Start)
        .ScorekeepUrl(
            model.QuizCode
            |> Option.map (fun code -> router.Link(Page.QuizRun code))
            |> Option.defaultValue ""
        )
        .SpectateUrl(
            model.QuizCode
            |> Option.map (fun code -> router.Link(Page.QuizSpectate code))
            |> Option.defaultValue ""
        )
        .LiveScoreUrl(
            model.QuizCode
            |> Option.map (fun code -> router.Link(Page.QuizLiveScore(code, Router.noModel)))
            |> Option.defaultValue ""
        )
        .QuizCode(model.QuizCode |> Option.defaultValue "", (fun code -> code |> Message.SetQuizCode |> dispatch))
        .CreateQuizModal(CreateQuizForm.view model.CreateQuizForm (dispatch << Message.CreateQuiz))
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

let linkToQuizPage (router: Router<Page, Model, Message>) =
    fun quizCode -> router.Link <| Page.QuizSpectate quizCode

let view model dispatch =
    Main()
        .Menu(
            concat {
                menuItem model Home "Home"
                menuItem model (QuizRun "Example") "Quiz Example"
            }
        )
        .Body(
            cond model.page
            <| function
                | Home -> homePage model dispatch
                | QuizSpectate code
                | QuizRun code ->
                    match model.Quiz with
                    | None -> Node.Empty()
                    | Some quizModel ->
                        page (linkToQuizPage router) quizModel (fun quizMsg -> dispatch (QuizMessage quizMsg))
                | QuizLiveScore (quizCode, pageModel) ->
                    let model =
                        { pageModel.Model with Code = quizCode }

                    LiveScorePage.page model
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

type Dependencies =
    { GetQuiz: GetQuiz
      SaveQuiz: SaveQuiz }

let runQuizCapabilities dependencies : RunQuizCapabilityProvider =
    let onlyQuizmastersAndScorekeepers user cap =
        match user with
        | Quizmaster -> cap
        | Scorekeeper -> cap
        | Spectator -> None
        | Quizzer _ -> None

    let onlyForCurrentQuizzer currentQuizzer originalCap =
        currentQuizzer
        |> Option.bind (fun _ -> originalCap)

    let addQuizzer user =
        let originalCap =
            AddQuizzer_Pipeline.addQuizzerAsync dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let removeQuizzer user =
        let originalCap =
            RemoveQuizzer_Pipeline.removeQuizzer dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let answerCorrectly user quizzerOpt =
        let originalCap =
            AnswerCorrectly_Pipeline.answerCorrectly dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForCurrentQuizzer quizzerOpt

    let answersIncorrectly user quizzerOpt =
        let originalCap =
            AnswerIncorrectly.Pipeline.answerIncorrectly dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForCurrentQuizzer quizzerOpt

    let failAppealCap user quizzerOpt =
        let originalCap =
            FailAppeal.Pipeline.failAppeal dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForCurrentQuizzer quizzerOpt

    let clearAppealCap user quizzerOpt =
        let originalCap =
            ClearAppeal.Pipeline.clearAppeal dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForCurrentQuizzer quizzerOpt

    let changeCurrentQuestionCap user =
        let originalCap =
            ChangeCurrentQuestion_Pipeline.changeCurrentQuestionAsync dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let selectQuizzerCap user =
        let originalCap =
            SelectQuizzer_Pipeline.selectQuizzer dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let completeQuizCap user =
        CompleteQuiz.Pipeline.completeQuiz dependencies.GetQuiz dependencies.SaveQuiz
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let reopenQuizCap user =
        ReopenQuiz.Pipeline.reopenQuiz dependencies.GetQuiz dependencies.SaveQuiz
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    { AddQuizzer = addQuizzer
      RemoveQuizzer = removeQuizzer
      AnswerCorrectly = answerCorrectly
      AnswerIncorrectly = answersIncorrectly
      FailAppeal = failAppealCap
      ClearAppeal = clearAppealCap
      ChangeCurrentQuestion = changeCurrentQuestionCap
      SelectQuizzer = selectQuizzerCap
      CompleteQuiz = completeQuizCap
      ReopenQuiz = reopenQuizCap }

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val GetQuizAsync = Unchecked.defaultof<GetQuiz> with get, set

    [<Inject>]
    member val SaveQuizAsync = Unchecked.defaultof<SaveQuiz> with get, set

    [<Inject>]
    member val HubConnection = Unchecked.defaultof<HubConnection> with get, set

    [<Inject>]
    member val SaveNewQuiz = Unchecked.defaultof<SaveNewQuiz> with get, set

    [<Inject>]
    member val TryGetQuiz = Unchecked.defaultof<TryGetQuiz> with get, set

    override this.Program =
        let hubConnection = this.HubConnection

        let connectToQuizEvents quizCode previousQuizCode =
            hubConnection.InvokeAsync("ConnectToQuiz", quizCode, previousQuizCode, CancellationToken.None)
            |> Async.AwaitTask

        let onQuizEvent =
            fun handler ->
                let handlerTask =
                    fun event -> handler event |> Async.startAsPlainTask

                hubConnection.On<RunQuizEvent>(nameof clientStub.RunQuizEventOccurred, handlerTask)
                |> ignore

        let publishQuizEvent =
            fun methodName quiz event ->
                hubConnection.InvokeAsync(methodName, quiz, event, CancellationToken.None)
                |> Async.AwaitTask

        let spectateQUiz quizCode =
            Page.QuizRun quizCode
            |> router.getRoute
            |> this.NavigationManager.NavigateTo

        let availableCapabilities =
            runQuizCapabilities
                { SaveQuiz = this.SaveQuizAsync
                  GetQuiz = this.GetQuizAsync }

        let navigate currentPage toPage =
            let quizCodeOpt =
                previousQuizCode currentPage

            do
                quizCodeOpt
                |> Option.map (disconnectFromHub this.HubConnection)
                |> Option.defaultValue (Async.retn ())
                |> Async.map (fun _ ->
                    if true then
                        this.NavigationManager.NavigateTo
                        <| router.Link toPage
                    else
                        ())
                |> Async.StartImmediate



        let update =
            update
                connectToQuizEvents
                onQuizEvent
                publishQuizEvent
                this.GetQuizAsync
                this.SaveNewQuiz
                this.TryGetQuiz
                spectateQUiz
                hubConnection
                navigate
                availableCapabilities

        Program.mkProgram
            (fun _ ->
                hubConnection.StartAsync() |> ignore
                initModel, Cmd.none)
            update
            view
        |> Program.withRouter router
#if DEBUG
        |> Program.withConsoleTrace
        |> Program.withHotReload
#endif
