module FreeMethodist.BibleQuizTracker.Server.QuizPage

open System
open System.Threading
open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server
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

type PublishEventError =
    | FormError of string
    | RemoteError of exn

type Message =
    | StartListeningToEvents
    | OverrideScore of int * TeamPosition
    | RefreshQuiz
    | DoNothing
    | MoveToDifferentQuestion of int
    | AsyncCommandError of PublishEventError

type ExternalMessage = Error of string

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

let init quizCode =
    { JoiningQuizzer = ""
      Code = quizCode
      TeamOne = { Name = ""; Score = 0; Quizzers = [] }
      TeamTwo = { Name = ""; Score = 0; Quizzers = [] }
      JumpOrder = []
      CurrentQuestion = 1
      CurrentUser = ""
      JumpState = Unlocked
      CurrentJumpPosition = 0 },
    Cmd.ofMsg Message.StartListeningToEvents


let subscribe (hubConnection: HubConnection) =
    let sub dispatch =
        hubConnection.On<QuizzerEntered>(
            "EnteredQuiz",
            (fun (msg: QuizzerEntered) -> dispatch (Message.RefreshQuiz) |> ignore)
        )
        |> ignore

        hubConnection.On<TeamScoreChanged>(
            nameof
                Unchecked.defaultof<QuizHub.Client>
                    .TeamScoreChanged,
            (fun (msg: TeamScoreChanged) -> dispatch (Message.RefreshQuiz) |> ignore)
        )
        |> ignore

        hubConnection.On<QuestionChanged>(
            nameof
                Unchecked.defaultof<QuizHub.Client>
                    .QuestionChanged,
            (fun (msg) -> dispatch (Message.RefreshQuiz) |> ignore)
        )
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

let private hubStub =
    Unchecked.defaultof<QuizHub.Hub>

let update
    (connectToQuizEvents: ConnectToQuizEvents)
    (publishEvent: PublishEventTask)
    getQuiz
    saveQuiz
    msg
    model
    =

    let publishEventCmd methodName event =
        Cmd.OfAsync.either
            (fun _ -> publishEvent methodName event)
            ()
            (fun _ -> Message.RefreshQuiz)
            (fun er -> er |> RemoteError |> Message.AsyncCommandError)

    match msg with
    | StartListeningToEvents ->
        let task =
            async {
                do! connectToQuizEvents model.Code
                return RefreshQuiz
            }

        model, Cmd.OfAsync.result task, None
    | OverrideScore (score, teamPosition) ->
        let result =
            overrideScore getQuiz saveQuiz model score teamPosition

        let overrideScoreError error =
            match error with
            | OverrideScoreErrors.DomainError error -> $"Quiz is in the wrong state: {error}"
            | OverrideScoreErrors.FormError error -> error

        match result with
        | Ok event ->
            let cmd =
                publishEventCmd (nameof hubStub.TeamScoreChanged) event

            model, cmd, None
        | Result.Error error -> model, Cmd.none, Some(ExternalMessage.Error(overrideScoreError error))
    | RefreshQuiz ->
        getQuiz model.Code
        |> refreshModel
        |> fun refreshedModel -> refreshedModel, Cmd.none, None
    | DoNothing -> model, Cmd.none, None
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

        let moveQuestionErrorMessage error =
            match error with
            | MoveQuestionError.FormError er -> er
            | MoveQuestionError.QuizError er -> $"Wrong Quiz State: {er}"

        match workflowResult with
        | Ok event ->
            let cmd =
                publishEventCmd (nameof hubStub.SendQuestionChanged) event

            model, cmd, None
        | Result.Error event ->
            model,
            Cmd.none,
            (moveQuestionErrorMessage event
             |> ExternalMessage.Error
             |> Some)

    | Message.AsyncCommandError error ->
        let errorMessage =
            match error with
            | PublishEventError.FormError er -> er
            | RemoteError exn -> exn.Message

        model, Cmd.none, errorMessage |> ExternalMessage.Error |> Some

type private quizPage = Template<"wwwroot/Quiz.html">

let private teamView position ((teamModel, jumpOrder): TeamModel * string list) (dispatch: Dispatch<Message>) =
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
                            | 0 -> "is-invisible"
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
        .CurrentQuizzer(
            if model.CurrentJumpPosition = 0
               || model.JumpOrder.IsEmpty then
                ""
            else
                model.JumpOrder.Item model.CurrentJumpPosition
        )
        .JumpLockToggleAction(
            match model.JumpState with
            | Locked -> "Unlock"
            | Unlocked -> "Lock"
        )
        .TeamOneName(model.TeamOne.Name)
        .TeamTwoName(model.TeamTwo.Name)
        .SetAddQuizzerName("Quizzer1", (fun name -> ()))
        .CancelAddQuizzer(fun _ -> ())
        .AddQuizzerActive("")
        .AddQuizzer(fun _ -> ())
        .SetAddQuizzerTeamOne(fun _ -> ())
        .SetAddQuizzerTeamTwo(fun _ -> ())
        .Elt()
