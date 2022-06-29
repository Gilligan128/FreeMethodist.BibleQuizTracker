﻿module FreeMethodist.BibleQuizTracker.Server.QuizPage

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
type QuizzerModel = {
    Name: string
    Score: int
    ConnectionStatus: ConnectionStatus
}
type TeamModel = {
    Name: string
    Score: int
    Quizzers: QuizzerModel list
}

type Model =
    { CurrentQuizzer: Quizzer
      JoiningQuizzer: string
      Code: string
      TeamOne: TeamModel
      TeamTwo: TeamModel
      JumpOrder : string list
     }

let initModel =
    { CurrentQuizzer = ""
      JoiningQuizzer = ""
      Code = "TEST"
      TeamOne = { Name = "LEFT"; Score = 0; Quizzers = [] }
      TeamTwo = { Name = "RIGHT"; Score = 0; Quizzers = [] }
      JumpOrder = []
      }

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
    | QuizzerEntered quizzerEntered ->
        { model with CurrentQuizzer = quizzerEntered.Quizzer }, Cmd.none


let page (model: Model) (dispatch: Dispatch<Message>) : Node =
    div {
        attr.``class`` "column"
        
        p { $"Quizzer: {model.CurrentQuizzer}" }

        input {
            attr.placeholder "Joining Quizzer:"
            bind.input.string model.JoiningQuizzer (fun n -> dispatch (SetJoiningQuizzer n))
        }

        button {
            on.click (fun _ -> dispatch (JoinQuiz))
            "Join"
        }
    }
