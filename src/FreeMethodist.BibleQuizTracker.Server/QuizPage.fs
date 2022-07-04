﻿module FreeMethodist.BibleQuizTracker.Server.QuizPage

open System
open System.Threading
open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Api
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Pipeline
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain
open Microsoft.AspNetCore.SignalR.Client
open Microsoft.FSharp.Control
open Microsoft.FSharp.Core
open Elmish

type ConnectionStatus =
    | Connected
    | Disconnected of DateTimeOffset
    | Unknown

type QuizzerModel =
    { Name: string
      Score: int
      ConnectionStatus: ConnectionStatus }

type TeamModel =
    { Name: string
      Score: int
      Quizzers: QuizzerModel list }

type JumpState =
    | Locked
    | Unlocked

type Model =
    { JoiningQuizzer: string
      Code: string
      TeamOne: TeamModel
      TeamTwo: TeamModel
      JumpOrder: string list
      CurrentQuestion: int
      CurrentUser: string
      JumpState: JumpState
      CurrentJumpPosition: int }

type Message =
    | SetJoiningQuizzer of string
    | JoinQuiz
    | OverrideScore of int * TeamPosition
    | RefreshQuiz of QuizCode
    | DoNothing

let exampleQuiz code =

    result {
        let! teamOneScore = TeamScore.create 20
        let! teamTwoScore = TeamScore.create 40
        let! jimScore = TeamScore.create 20
        let! jinaScore = TeamScore.create 40

        return
            { Code = code
              TeamOne =
                { Name = "LEFT"
                  Score = teamOneScore
                  Captain = None
                  Quizzers =
                    [ { Name = "Jim"
                        Score = jimScore
                        Participation = In }
                      { Name = "Jim"
                        Score = jimScore
                        Participation = In } ] }
              TeamTwo =
                { Name = "RIGHT"
                  Score = teamTwoScore
                  Captain = None
                  Quizzers =
                    [ { Name = "Jina"
                        Score = jinaScore
                        Participation = In }
                      { Name = "Jina"
                        Score = jinaScore
                        Participation = In } ] }

              Questions = [] }
    }

let private getQuizStub (quiz) = fun code -> quiz

let private refreshModel (quiz: RunningTeamQuiz) =
    let refreshQuizzer (quizzer: QuizzerState) =
        { Name = quizzer.Name
          Score = TeamScore.value quizzer.Score
          ConnectionStatus = Unknown }

    let refreshTeam (team: QuizTeamState) =
        { Name = team.Name
          Score = TeamScore.value team.Score
          Quizzers = team.Quizzers |> List.map refreshQuizzer }

    { Code = quiz.Code
      TeamOne = quiz.TeamOne |> refreshTeam
      TeamTwo = quiz.TeamTwo |> refreshTeam
      CurrentQuestion = 3
      CurrentJumpPosition = 1
      CurrentUser = "Quizmaster"
      JoiningQuizzer = ""
      JumpOrder = [ "Jim"; "Juni"; "John" ]
      JumpState = Unlocked }

let initModel =

    let emptyModel =
        { JoiningQuizzer = ""
          Code = ""
          TeamOne = { Name = ""; Score = 0; Quizzers = [] }
          TeamTwo = { Name = ""; Score = 0; Quizzers = [] }
          JumpOrder = []
          CurrentQuestion = 3
          CurrentUser = ""
          JumpState = Unlocked
          CurrentJumpPosition = 0 }

    match exampleQuiz "TEST" with
    | Ok quiz -> refreshModel quiz
    | Error message -> emptyModel


let subscribe (signalRConnection: HubConnection) (initial: Model) =
    let sub dispatch =
        signalRConnection.On<QuizzerEntered>(
            "EnteredQuiz",
            (fun (msg: QuizzerEntered) -> dispatch (Message.RefreshQuiz msg.Quiz) |> ignore)
        )
        |> ignore

        signalRConnection.On<TeamScoreChanged>(
            nameof
                Unchecked.defaultof<QuizHub.Client>
                    .HandleQuizEvent,
            (fun (msg: TeamScoreChanged) -> dispatch (Message.RefreshQuiz msg.Quiz) |> ignore)
        )
        |> ignore

        signalRConnection.InvokeAsync(nameof Unchecked.defaultof<QuizHub.Hub>.ConnectToQuiz, initial.Code)
        |> Async.AwaitTask
        |> ignore

    Cmd.ofSub sub

type OverrideScoreErrors =
    | DomainError of OverrideTeamScore.Error
    | FormError of string

let private overrideScore (model: Model) (score: int) (team: TeamPosition) =
    result {
        let! exampleQuiz =
            exampleQuiz model.Code
            |> Result.mapError (FormError)

        let getQuiz =
            getQuizStub (Running exampleQuiz)

        let! newScore =
            TeamScore.create score
            |> Result.mapError (FormError)

        let command =
            { Quiz = model.Code
              Data = { Team = team; NewScore = newScore }
              User = Quizmaster }

        return!
            overrideTeamScore (getQuiz) ignore command
            |> Result.mapError (DomainError)
    }

let update (hubConnection: HubConnection) msg model =
    match msg with
    | SetJoiningQuizzer quizzer -> { model with JoiningQuizzer = quizzer }, Cmd.none
    | JoinQuiz ->
        let hubMessage =
            { Timestamp = DateTimeOffset.Now
              Quizzer = model.JoiningQuizzer
              Quiz = "123" }

        hubConnection.InvokeAsync(nameof Unchecked.defaultof<QuizHub.Hub>.EnterQuiz, hubMessage)
        |> ignore

        { model with JoiningQuizzer = "" }, Cmd.none
    | OverrideScore (score, teamPosition) ->
        let result =
            overrideScore model score teamPosition

        match result with
        | Ok event ->
            let task =
                (fun _ ->
                    hubConnection.InvokeAsync(nameof Unchecked.defaultof<QuizHub.Hub>.TeamScoreChanged, event)
                    |> Async.AwaitTask)

            let cmd =
                Cmd.OfAsync.either task ignore (fun _ -> Message.RefreshQuiz event.Quiz) (fun er -> Message.DoNothing)

            model, cmd
        | Error error -> model, Cmd.none
    | RefreshQuiz quizCode ->
        exampleQuiz quizCode
        |> Result.map refreshModel
        |> function
            | Ok refreshedModel -> refreshedModel, Cmd.none
            | Error _ -> model, Cmd.none
    | DoNothing -> model, Cmd.none


type quizPage = Template<"wwwroot/Quiz.html">

let teamView position ((teamModel, jumpOrder): TeamModel * string list) (dispatch: Dispatch<Message>) =
    quizPage
        .Team()
        .Name(teamModel.Name)
        .Score(string teamModel.Score, (fun score -> dispatch (OverrideScore(int score, position))))
        .Quizzers(
            concat {
                for quizzer in teamModel.Quizzers do
                    let jumpPosition =
                        jumpOrder
                        |> Seq.tryFindIndex (fun q -> q = quizzer.Name)
                        |> Option.map ((+) 1)
                        |> function
                            | Some v -> v
                            | None -> 0

                    quizPage
                        .Quizzer()
                        .Name(quizzer.Name)
                        .Score(string quizzer.Score)
                        .JumpOrder(jumpPosition |> string)
                        .HiddenClass(
                            match jumpPosition with
                            | 0 -> "is-hidden"
                            | _ -> ""
                        )
                        .Elt()
            }
        )
        .Elt()

let page (model: Model) (dispatch: Dispatch<Message>) =
    quizPage()
        .QuizCode(model.Code)
        .CurrentUser(model.CurrentUser)
        .TeamOne(teamView TeamPosition.TeamOne (model.TeamOne, model.JumpOrder) dispatch)
        .TeamTwo(teamView TeamPosition.TeamTwo (model.TeamTwo, model.JumpOrder) dispatch)
        .CurrentQuestion(string model.CurrentQuestion)
        .CurrentQuizzer(model.JumpOrder.Item model.CurrentJumpPosition)
        .JumpLockToggleAction(
            match model.JumpState with
            | Locked -> "Unlock"
            | Unlocked -> "Lock"
        )
        .Elt()
