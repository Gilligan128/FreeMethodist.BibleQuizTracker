module FreeMethodist.BibleQuizTracker.Server.QuizPage

open System
open System.Threading
open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open Microsoft.AspNetCore.SignalR.Client
open Microsoft.FSharp.Core
open Elmish

type ConnectionStatus =
    | Connected
    | Disconnected of DateTimeOffset

type QuizzerModel =
    { Name: string
      Score: int
      ConnectionStatus: ConnectionStatus }

type TeamModel =
    { Name: string
      Score: int
      Quizzers: QuizzerModel list }

type Model =
    { CurrentQuizzer: Quizzer
      JoiningQuizzer: string
      Code: string
      TeamOne: TeamModel
      TeamTwo: TeamModel
      JumpOrder: string list
      CurrentQuestion: int
      CurrentUser: string }

let initModel =
    { CurrentQuizzer = ""
      JoiningQuizzer = ""
      Code = "TEST"
      TeamOne =
        { Name = "LEFT"
          Score = 0
          Quizzers =
            [ { Name = "Jim"
                Score = 20
                ConnectionStatus = Connected } ] }
      TeamTwo =
        { Name = "RIGHT"
          Score = 0
          Quizzers =
            [ { Name = "Jina"
                Score = 40
                ConnectionStatus = Connected } ] }
      JumpOrder = []
      CurrentQuestion = 3
      CurrentUser = "Quizmaster" }

type Message =
    | SetJoiningQuizzer of string
    | JoinQuiz
    | QuizzerEntered of QuizzerEntered

//I believe this should correctly setup the signalR subscription
let subscribe (signalRConnection: HubConnection) initial =
    let sub dispatch =
        signalRConnection.On<QuizzerEntered>(
            "EnteredQuiz",
            (fun msg -> dispatch (Message.QuizzerEntered msg) |> ignore)
        )
        |> ignore

    Cmd.ofSub sub

let update (hubConnection: HubConnection) msg model =
    match msg with
    | SetJoiningQuizzer quizzer -> { model with JoiningQuizzer = quizzer }, Cmd.none
    | JoinQuiz ->
        let hubMessage =
            { Timestamp = DateTimeOffset.Now
              Quizzer = model.JoiningQuizzer
              Quiz = "123" }

        hubConnection.InvokeAsync("EnterQuiz", hubMessage)
        |> ignore

        { model with JoiningQuizzer = "" }, Cmd.none
    | QuizzerEntered quizzerEntered -> { model with CurrentQuizzer = quizzerEntered.Quizzer }, Cmd.none

type quizPage = Template<"wwwroot/Quiz.html">

let page (model: Model) (dispatch: Dispatch<Message>) =
    quizPage()
        .CurrentUser(model.CurrentUser)
        .TeamOne(model.TeamOne.Name)
        .TeamOneQuizzers(concat {
            for quizzer in model.TeamOne.Quizzers do
                quizPage
                    .quizzer()
                    .Name(quizzer.Name)
                    .Score(string quizzer.Score)
                    .Elt()
        })
        .TeamTwo(model.TeamTwo.Name)
        .TeamTwoQuizzers(concat {
            for quizzer in model.TeamTwo.Quizzers do
                quizPage
                    .quizzer()
                    .Name(quizzer.Name)
                    .Score(string quizzer.Score)
                    .Elt()
        })
        .Elt()
