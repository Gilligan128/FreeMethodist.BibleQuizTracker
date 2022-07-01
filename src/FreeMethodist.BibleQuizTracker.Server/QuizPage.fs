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
    { CurrentQuizzer = "Jim"
      JoiningQuizzer = ""
      Code = "TEST"
      TeamOne =
        { Name = "LEFT"
          Score = 20
          Quizzers =
            [ { Name = "Jim"
                Score = 20
                ConnectionStatus = Connected }
              { Name = "John"
                Score = 0
                ConnectionStatus = Connected }
              { Name = "Kinda Long Name"
                Score = 0
                ConnectionStatus = Connected } ] }
      TeamTwo =
        { Name = "RIGHT"
          Score = 40
          Quizzers =
            [ { Name = "Jina"
                Score = 40
                ConnectionStatus = Connected }
              { Name = "Juni"
                Score = 0
                ConnectionStatus = Connected } ] }
      JumpOrder = [ "Jim"; "Juni"; "John" ]
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

let teamView ((teamModel, jumpOrder): TeamModel * string list) (dispatch: Dispatch<Message>) =
    quizPage
        .Team()
        .Name(teamModel.Name)
        .Score(string teamModel.Score)
        .Quizzers(
            concat {
                for quizzer in teamModel.Quizzers do
                    quizPage
                        .Quizzer()
                        .Name(quizzer.Name)
                        .Score(string quizzer.Score)
                        .JumpOrder(
                            jumpOrder
                            |> Seq.tryFindIndex (fun q -> q = quizzer.Name)
                            |> Option.map ((+) 1)
                            |> function
                                | Some v -> v
                                | None -> 0
                            |> string
                        )
                        .Elt()
            }
        )
        .Elt()

let page (model: Model) (dispatch: Dispatch<Message>) =
    quizPage()
        .QuizCode(model.Code)
        .CurrentUser(model.CurrentUser)
        .TeamOne(teamView (model.TeamOne, model.JumpOrder) dispatch)
        .TeamTwo(teamView (model.TeamTwo, model.JumpOrder) dispatch)
        .CurrentQuestion(string model.CurrentQuestion)
        .CurrentQuizzer(model.CurrentQuizzer)
        .Elt()
