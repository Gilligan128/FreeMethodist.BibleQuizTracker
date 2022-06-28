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
    | [<EndPoint "/counter">] Counter
    | [<EndPoint "/quiz">] Quiz

/// The Elmish application's model.
type Model =
    { page: Page
      counter: int
      error: string option
      quiz: QuizPage.Model }

and Book =
    { title: string
      author: string
      publishDate: DateTime
      isbn: string }

let initModel =
    { page = Home
      counter = 0
      error = None
      quiz = QuizPage.initModel }

/// Remote service definition.
type BookService =
    { /// Get the list of all books in the collection.
      getBooks: unit -> Async<Book []>

      /// Add a book in the collection.
      addBook: Book -> Async<unit>

      /// Remove a book from the collection, identified by its ISBN.
      removeBookByIsbn: string -> Async<unit>

      /// Sign into the application.
      signIn: string * string -> Async<option<string>>

      /// Get the user's name, or None if they are not authenticated.
      getUsername: unit -> Async<string>

      /// Sign out from the application.
      signOut: unit -> Async<unit> }

    interface IRemoteService with
        member this.BasePath = "/books"

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | Increment
    | Decrement
    | SetCounter of int
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
    | Increment -> { model with counter = model.counter + 1 }, Cmd.none
    | Decrement -> { model with counter = model.counter - 1 }, Cmd.none
    | SetCounter value -> { model with counter = value }, Cmd.none
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

let counterPage model dispatch =
    Main
        .Counter()
        .Decrement(fun _ -> dispatch Decrement)
        .Increment(fun _ -> dispatch Increment)
        .Value(model.counter, (fun v -> dispatch (SetCounter v)))
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

let view model dispatch =
    Main()
        .Menu(
            concat {
                menuItem model Home "Home"
                menuItem model Counter "Counter"
            }
        )
        .Body(
            cond model.page
            <| function
                | Home -> homePage model dispatch
                | Counter -> counterPage model dispatch
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

type MyApp() =
    inherit ProgramComponent<Model, Message>()
                                                                    
    override this.Program =
        Program.mkProgram (fun _ ->
            hubConnection.StartAsync() |> ignore
            initModel, Cmd.none) update view
        |> Program.withRouter router
        |> Program.withSubscription subscription
#if DEBUG
        |> Program.withHotReload
#endif
