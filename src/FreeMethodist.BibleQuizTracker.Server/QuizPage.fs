module FreeMethodist.BibleQuizTracker.Server.QuizPage

open System
open System.Threading
open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Api
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Pipeline
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain
open Microsoft.AspNetCore.SignalR.Client
open Microsoft.FSharp.Control
open Microsoft.FSharp.Core
open Elmish
open FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Pipeline
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi


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
    | OverrideScore of int * TeamPosition
    | RefreshQuiz of QuizCode
    | DoNothing
    | MoveToDifferentQuestion of int

let private refreshModel (quiz: TeamQuiz) =
    let refreshQuizzer (quizzer: QuizzerState) =
        { Name = quizzer.Name
          Score = TeamScore.value quizzer.Score
          ConnectionStatus = Unknown }

    let refreshTeam (team: QuizTeamState) =
        { Name = team.Name
          Score = TeamScore.value team.Score
          Quizzers = team.Quizzers |> List.map refreshQuizzer }

    match quiz with
    | Running runningQuiz ->
        { Code = runningQuiz.Code
          TeamOne = runningQuiz.TeamOne |> refreshTeam
          TeamTwo = runningQuiz.TeamTwo |> refreshTeam
          CurrentQuestion = PositiveNumber.value runningQuiz.CurrentQuestion
          CurrentJumpPosition = 1
          CurrentUser = "Quizmaster"
          JoiningQuizzer = ""
          JumpOrder = [ "Jim"; "Juni"; "John" ]
          JumpState = Unlocked }
    | TeamQuiz.Completed _
    | Official _
    | Unvalidated _ ->
        { JoiningQuizzer = ""
          Code = ""
          TeamOne = { Name = ""; Score = 0; Quizzers = [] }
          TeamTwo = { Name = ""; Score = 0; Quizzers = [] }
          JumpOrder = []
          CurrentQuestion = 1
          CurrentUser = ""
          JumpState = Unlocked
          CurrentJumpPosition = 0 }

let initModel getQuiz =
    let quiz = getQuiz "TEST"
    refreshModel quiz

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
    | DomainError of QuizStateError
    | FormError of string

type MoveQuestionError =
    | FormError of string
    | QuizError of QuizStateError

let private overrideScore getQuiz saveQuiz (model: Model) (score: int) (team: TeamPosition) =
    result {
        let! newScore =
            TeamScore.create score
            |> Result.mapError (OverrideScoreErrors.FormError)

        let command =
            { Quiz = model.Code
              Data = { Team = team; NewScore = newScore }
              User = Quizmaster }

        return!
            overrideTeamScore getQuiz saveQuiz command
            |> Result.mapError (DomainError)
    }

let publishEvent (hubConnection: HubConnection) (hubMethod) event =
    let method =
        hubMethod Unchecked.defaultof<QuizHub.Hub>

    hubConnection.InvokeAsync(nameof method, event, None)
    |> Async.AwaitTask

let createAsyncCommand task quiz =
    Cmd.OfAsync.either task ignore (fun _ -> Message.RefreshQuiz quiz) (fun er -> Message.DoNothing)

let update (hubConnection: HubConnection) getQuiz saveQuiz msg model =
    let publishEvent = fun event -> publishEvent hubConnection event
    match msg with
    | OverrideScore (score, teamPosition) ->
        let result =
            overrideScore getQuiz saveQuiz model score teamPosition

        match result with
        | Ok event ->
            let task =
                (fun _ -> publishEvent  (fun hub -> hub.TeamScoreChanged) event)

            let cmd = createAsyncCommand task event.Quiz
            model, cmd
        | Error error -> model, Cmd.none
    | RefreshQuiz quizCode ->
        getQuiz quizCode
        |> refreshModel
        |> fun refreshedModel -> refreshedModel, Cmd.none
    | DoNothing -> model, Cmd.none
    | MoveToDifferentQuestion questionNumber ->
        let workflowResult =
            result {
                let! questionNumber =
                    PositiveNumber.create questionNumber "QuestionNumber"
                    |> Result.mapError MoveQuestionError.FormError

                return!
                    moveQuizToQuestion
                        getQuiz
                        saveQuiz
                        { Quiz = model.Code
                          User = Quizmaster
                          Data = { Question = questionNumber } }
                    |> Result.mapError MoveQuestionError.QuizError
            }

        match workflowResult with
        | Ok event ->
            let task =
                (fun _ -> publishEvent (fun hub -> hub.QuestionChanged) event)

            let cmd = createAsyncCommand task event.Quiz

            model, cmd
        | Error event -> model, Cmd.none

type quizPage = Template<"wwwroot/Quiz.html">

let teamView position ((teamModel, jumpOrder): TeamModel * string list) (dispatch: Dispatch<Message>) =
    quizPage
        .Team()
        .Name(teamModel.Name)
        .Score(teamModel.Score, (fun score -> dispatch (OverrideScore(score, position))))
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
        .NextQuestion(fun _ -> dispatch (MoveToDifferentQuestion(model.CurrentQuestion + 1)))
        .UndoQuestion(fun _ -> dispatch (MoveToDifferentQuestion(Math.Max(model.CurrentQuestion - 1, 1))))
        .CurrentQuizzer(model.JumpOrder.Item model.CurrentJumpPosition)
        .JumpLockToggleAction(
            match model.JumpState with
            | Locked -> "Unlock"
            | Unlocked -> "Lock"
        )
        .Elt()
