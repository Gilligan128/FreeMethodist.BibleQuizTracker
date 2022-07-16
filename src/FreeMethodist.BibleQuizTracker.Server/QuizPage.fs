module FreeMethodist.BibleQuizTracker.Server.QuizPage

open System
open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.AddQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Workflow
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Pipeline
open FreeMethodist.BibleQuizTracker.Server.RemoveQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.SelectQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Pipeline
open Microsoft.AspNetCore.SignalR.Client
open Microsoft.FSharp.Core
open FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Pipeline


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

type AddQuizzerModel =
    | Active of string * TeamPosition
    | Inert

type Model =
    { JoiningQuizzer: string
      Code: string
      TeamOne: TeamModel
      TeamTwo: TeamModel
      JumpOrder: string list
      CurrentQuestion: int
      CurrentUser: User
      JumpState: JumpState
      AddQuizzer: AddQuizzerModel
      CurrentQuizzer: Quizzer }

type PublishEventError =
    | FormError of string
    | RemoteError of exn

type AddQuizzerMessage =
    | Start
    | Cancel
    | Submit
    | SetName of string
    | SetTeam of TeamPosition

type Message =
    | ConnectToQuizEvents
    | OverrideScore of int * TeamPosition
    | RefreshQuiz
    | MoveToDifferentQuestion of int
    | PublishEventError of PublishEventError
    | AddQuizzer of AddQuizzerMessage
    | RemoveQuizzer of Quizzer * TeamPosition
    | SelectQuizzer of Quizzer

type ExternalMessage = Error of string

let public emptyModel =
    { JoiningQuizzer = ""
      Code = ""
      TeamOne = { Name = ""; Score = 0; Quizzers = [] }
      TeamTwo = { Name = ""; Score = 0; Quizzers = [] }
      JumpOrder = []
      CurrentQuestion = 1
      CurrentUser = User.Quizmaster
      JumpState = Unlocked
      AddQuizzer = Inert
      CurrentQuizzer = "" }

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
        { emptyModel with
            Code = runningQuiz.Code
            TeamOne = runningQuiz.TeamOne |> refreshTeam
            TeamTwo = runningQuiz.TeamTwo |> refreshTeam
            CurrentQuestion = PositiveNumber.value runningQuiz.CurrentQuestion
            CurrentQuizzer = runningQuiz.CurrentQuizzer
            CurrentUser = Quizmaster
            JoiningQuizzer = ""
            JumpOrder = [ "Jim"; "Juni"; "John" ]
            JumpState = Unlocked }
    | TeamQuiz.Completed _
    | Official _
    | Unvalidated _ -> emptyModel

let init quizCode =
    { emptyModel with Code = quizCode }, Cmd.ofMsg Message.ConnectToQuizEvents

type OverrideScoreErrors =
    | DomainError of QuizStateError
    | FormError of string

type MoveQuestionError =
    | FormError of string
    | QuizError of QuizStateError

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

        hubConnection.On<CurrentQuestionChanged>(
            nameof
                Unchecked.defaultof<QuizHub.Client>
                    .QuestionChanged,
            (fun (msg) -> dispatch (Message.RefreshQuiz) |> ignore)
        )
        |> ignore

    Cmd.ofSub sub

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

let update (connectToQuizEvents: ConnectToQuizEvents) (publishEvent: PublishEventTask) getQuiz saveQuiz msg model =
    let publishEventCmd methodName event =
        Cmd.OfAsync.either
            (fun _ -> publishEvent methodName event)
            ()
            (fun _ -> Message.RefreshQuiz)
            (fun er -> er |> RemoteError |> Message.PublishEventError)

    let externalErrorMessage message =
        message |> ExternalMessage.Error |> Some

    match msg with
    | ConnectToQuizEvents ->
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
        | Result.Error error -> model, Cmd.none, overrideScoreError error |> externalErrorMessage
    | RefreshQuiz ->
        getQuiz model.Code
        |> refreshModel
        |> fun refreshedModel -> refreshedModel, Cmd.none, None
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

    | Message.PublishEventError error ->
        let errorMessage =
            match error with
            | PublishEventError.FormError er -> er
            | RemoteError exn -> exn.Message

        model, Cmd.none, errorMessage |> ExternalMessage.Error |> Some
    | Message.AddQuizzer Cancel -> { model with AddQuizzer = Inert }, Cmd.none, None
    | Message.AddQuizzer Start ->
        let addQuizzerState =
            match model.AddQuizzer with
            | Inert -> Active("", TeamOne)
            | Active (name, team) -> Active(name, team)

        { model with AddQuizzer = addQuizzerState }, Cmd.none, None
    | AddQuizzer (SetName name) ->
        let addQuizzerModel =
            match model.AddQuizzer with
            | Inert -> Inert
            | Active (_, team) -> Active(name, team)

        { model with AddQuizzer = addQuizzerModel }, Cmd.none, None
    | AddQuizzer (SetTeam teamPosition) ->
        let addQuizzerModel =
            match model.AddQuizzer with
            | Inert -> Inert
            | Active (name, _) -> Active(name, teamPosition)

        { model with AddQuizzer = addQuizzerModel }, Cmd.none, None
    | AddQuizzer Submit ->
        match model.AddQuizzer with
        | AddQuizzerModel.Active (name, team) ->
            let command: AddQuizzer.Command =
                { Quiz = model.Code
                  Data = { Name = name; Team = team }
                  User = model.CurrentUser }

            let result =
                AddQuizzer_Pipeline.addQuizzer getQuiz saveQuiz command

            match result with
            | Ok event ->
                let cmd =
                    publishEventCmd (nameof hubStub.SendQuizzerParticipating) event

                { model with AddQuizzer = Inert }, cmd, None
            | Result.Error error ->
                let errorMessage =
                    match error with
                    | AddQuizzer.Error.QuizState quizStateError -> $"Wrong Quiz State: {quizStateError}"
                    | AddQuizzer.Error.QuizzerAlreadyAdded quizzer -> $"Quizzer {quizzer} already added"

                { model with AddQuizzer = Inert }, Cmd.none, errorMessage |> externalErrorMessage
        | AddQuizzerModel.Inert ->
            model,
            Cmd.none,
            ExternalMessage.Error "How are you even submitting from an inert AddQuizzer state?"
            |> Some
    | RemoveQuizzer (name, teamPosition) ->
        let withinQuizCommand: RemoveQuizzer.Command =
            { Quiz = model.Code
              Data = { Quizzer = name; Team = teamPosition }
              User = Quizmaster }

        let result =
            RemoveQuizzer_Pipeline.removeQuizzer getQuiz saveQuiz withinQuizCommand

        match result with
        | Ok event -> model, (publishEventCmd (nameof hubStub.SendQuizzerNoLongerParticipating) event), None
        | Result.Error error ->
            let errorMessage =
                match error with
                | RemoveQuizzer.QuizStateError quizStateError -> $"Wrong Quiz State: {quizStateError}"
                | RemoveQuizzer.QuizzerNotParticipating quizzer -> $"Quizzer {quizzer} is already not participating"

            model, Cmd.none, errorMessage |> ExternalMessage.Error |> Some
    | SelectQuizzer (name) ->
        let command: SelectQuizzer.Command =
            { Quiz = model.Code
              User = model.CurrentUser
              Data = { Quizzer = name } }

        let result =
            SelectQuizzer_Pipeline.selectQuizzer getQuiz saveQuiz command

        let errorResult (error: SelectQuizzer.Error) =
            match error with
            | SelectQuizzer.Error.QuizState quizStateError ->
                model,
                Cmd.none,
                $"Wrong Quiz State: {quizStateError}"
                |> externalErrorMessage
            | SelectQuizzer.Error.QuizzerAlreadyCurrent -> model, Cmd.none, None
            | SelectQuizzer.Error.QuizzerNotParticipating quizzer ->
                model,
                Cmd.none,
                $"Quizzer {quizzer} is not participating"
                |> externalErrorMessage

        match result with
        | Ok event -> model, publishEventCmd (nameof hubStub.SendCurrentQuizzerChanged) event, None
        | Result.Error error -> errorResult error

type private quizPage = Template<"wwwroot/Quiz.html">

let private teamView
    position
    ((teamModel, jumpOrder, currentQuizzer): TeamModel * string list * Quizzer)
    (dispatch: Dispatch<Message>)
    =
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
                        .Remove(fun _ -> dispatch (RemoveQuizzer(quizzer.Name, position)))
                        .Select(fun _ -> dispatch (SelectQuizzer(quizzer.Name)))
                        .BackgroundColor(
                            if quizzer.Name = currentQuizzer then
                                "has-background-grey-lighter"
                            else
                                "has-background-white-ter"
                        )
                        .Elt()
            }
        )
        .Elt()

let page (model: Model) (dispatch: Dispatch<Message>) =
    let isTeam teamOneValue teamTwoValue =
        match model.AddQuizzer with
        | Inert -> false
        | Active (_, TeamOne) -> teamOneValue
        | Active (_, TeamTwo) -> teamTwoValue

    quizPage()
        .QuizCode(model.Code)
        .CurrentUser(
            match model.CurrentUser with
            | Quizmaster -> "Quizmaster"
            | Spectator -> "Spectator"
            | Quizzer name -> name
            | Scorekeeper -> "Scorekeeper"
        )
        .TeamOne(teamView TeamPosition.TeamOne (model.TeamOne, model.JumpOrder, model.CurrentQuizzer) dispatch)
        .TeamTwo(teamView TeamPosition.TeamTwo (model.TeamTwo, model.JumpOrder, model.CurrentQuizzer) dispatch)
        .CurrentQuestion(string model.CurrentQuestion)
        .NextQuestion(fun _ -> dispatch (MoveToDifferentQuestion(model.CurrentQuestion + 1)))
        .UndoQuestion(fun _ -> dispatch (MoveToDifferentQuestion(Math.Max(model.CurrentQuestion - 1, 1))))
        .CurrentQuizzer(model.CurrentQuizzer)
        .JumpLockToggleAction(
            match model.JumpState with
            | Locked -> "Unlock"
            | Unlocked -> "Lock"
        )
        .TeamOneName(model.TeamOne.Name)
        .TeamTwoName(model.TeamTwo.Name)
        .AddQuizzerIsTeamOne(isTeam true false)
        .AddQuizzerIsTeamTwo(isTeam false true)
        .AddQuizzerName(
            (match model.AddQuizzer with
             | Active (name, _) -> name
             | Inert -> ""),
            (fun name ->
                dispatch (
                    AddQuizzerMessage.SetName name
                    |> Message.AddQuizzer
                ))
        )
        .AddQuizzerStart(fun _ -> dispatch (AddQuizzer Start))
        .AddQuizzerCancel(fun _ -> dispatch (AddQuizzer Cancel))
        .AddQuizzerActive(
            if model.AddQuizzer = Inert then
                ""
            else
                "is-active"
        )
        .AddQuizzerSubmit(fun _ -> dispatch (AddQuizzer Submit))
        .SetAddQuizzerTeamOne(fun _ -> dispatch (AddQuizzer(SetTeam TeamOne)))
        .SetAddQuizzerTeamTwo(fun _ -> dispatch (AddQuizzer(SetTeam TeamTwo)))
        .Elt()
