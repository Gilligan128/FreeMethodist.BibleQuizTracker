module FreeMethodist.BibleQuizTracker.Server.QuizPage

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
    | QuizzerEntered of QuizzerEntered
    | OverrideScore of int * TeamName

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

let getQuizStub (quiz) = fun code -> quiz

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
    | Ok quiz ->
        { JoiningQuizzer = ""
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
          CurrentUser = "Quizmaster"
          JumpState = Unlocked
          CurrentJumpPosition = 0 }
    | Error message -> emptyModel


let subscribe (signalRConnection: HubConnection) initial =
    let sub dispatch =
        signalRConnection.On<QuizzerEntered>(
            "EnteredQuiz",
            (fun msg -> dispatch (Message.QuizzerEntered msg) |> ignore)
        )
        |> ignore

    Cmd.ofSub sub

type OverrideScoreErrors =
    | DomainError of OverrideTeamScore.Error
    | FormError of string

let overrideScore (model: Model) (score: int) (team: TeamName) =
    result {
        let! exampleQuiz =
            exampleQuiz model.Code
            |> Result.mapError (FormError)

        let getQuiz =
            getQuizStub ( Running exampleQuiz)

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

        hubConnection.InvokeAsync("EnterQuiz", hubMessage)
        |> ignore

        { model with JoiningQuizzer = "" }, Cmd.none
    | QuizzerEntered quizzerEntered -> model, Cmd.none
    | OverrideScore (score, teamName) ->
        let result =
            overrideScore model score teamName

        match result with
        | Ok event -> { model with TeamOne = { model.TeamOne with Score = model.TeamOne.Score }}, Cmd.none
        | Error error -> model, Cmd.none


type quizPage = Template<"wwwroot/Quiz.html">

let teamView ((teamModel, jumpOrder): TeamModel * string list) (dispatch: Dispatch<Message>) =
    quizPage
        .Team()
        .Name(teamModel.Name)
        .Score(string teamModel.Score, (fun score -> dispatch (OverrideScore(int score, teamModel.Name))))
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
        .TeamOne(teamView (model.TeamOne, model.JumpOrder) dispatch)
        .TeamTwo(teamView (model.TeamTwo, model.JumpOrder) dispatch)
        .CurrentQuestion(string model.CurrentQuestion)
        .CurrentQuizzer(model.JumpOrder.Item model.CurrentJumpPosition)
        .JumpLockToggleAction(
            match model.JumpState with
            | Locked -> "Unlock"
            | Unlocked -> "Lock"
        )
        .Elt()
