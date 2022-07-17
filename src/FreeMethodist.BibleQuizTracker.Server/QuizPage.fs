module FreeMethodist.BibleQuizTracker.Server.QuizPage

open System
open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.AddQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
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
      CurrentQuizzer: Quizzer option }

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
    | ConnectToQuizEvents of Option<QuizCode>
    | OverrideScore of int * TeamPosition
    | RefreshQuiz
    | ChangeCurrentQuestion of int
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
      CurrentQuizzer = None }

let private refreshModel (quiz: Quiz) =
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
    | Quiz.Completed _
    | Official _
    | Unvalidated _ -> emptyModel

let init quizCode previousQuizCode =
    { emptyModel with Code = quizCode }, Message.ConnectToQuizEvents previousQuizCode |> Cmd.ofMsg

type OverrideScoreErrors =
    | DomainError of QuizStateError
    | FormError of string

type ChangeQuestionError =
    | FormError of string
    | QuizError of QuizStateError

let subscribe (hubConnection: HubConnection) =
    let sub (dispatch: Dispatch<Message>) =
        hubConnection.On<QuizzerEntered>("EnteredQuiz", (fun (msg: QuizzerEntered) -> dispatch (Message.RefreshQuiz)))
        |> ignore

        let clientStub =
            Unchecked.defaultof<QuizHub.Client>

        hubConnection.On<RunQuizEvent>(
            nameof clientStub.RunQuizEventOccurred,
            (fun (msg) -> dispatch Message.RefreshQuiz)
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

let update
    (connectToQuizEvents: ConnectToQuizEvents)
    (publishQuizEvent: PublishQuizEventTask)
    getQuiz
    getQuizAsync
    saveQuiz
    saveQuizAsync
    msg
    model
    =

    let commandToRefresh task =
        Cmd.OfAsync.either task () (fun _ -> Message.RefreshQuiz) (RemoteError >> Message.PublishEventError)
    
    let publishRunQuizEventCmd quiz (event: RunQuizEvent) =
        commandToRefresh (fun _ -> publishQuizEvent (nameof hubStub.SendRunQuizEventOccurred) quiz event)
   
    let externalErrorMessage message =
        message |> ExternalMessage.Error |> Some
    
    match msg with
    | ConnectToQuizEvents previousQuiz ->
        let task =
            async {
                do! connectToQuizEvents model.Code previousQuiz 
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
                publishRunQuizEventCmd event.Quiz (TeamScoreChanged event)

            model, cmd, None
        | Result.Error error -> model, Cmd.none, overrideScoreError error |> externalErrorMessage
    | RefreshQuiz ->
        getQuiz model.Code
        |> refreshModel
        |> fun refreshedModel -> refreshedModel, Cmd.none, None
    | ChangeCurrentQuestion questionNumber ->
        let workflowResult =
            result {
                let! questionNumber =
                    PositiveNumber.create questionNumber "QuestionNumber"
                    |> Result.mapError ChangeQuestionError.FormError

                return!
                    moveQuizToQuestion
                        getQuiz
                        saveQuiz
                        { Quiz = model.Code
                          User = Quizmaster
                          Data = { Question = questionNumber } }
                    |> Result.mapError ChangeQuestionError.QuizError
            }

        let moveQuestionErrorMessage error =
            match error with
            | ChangeQuestionError.FormError er -> er
            | ChangeQuestionError.QuizError er -> $"Wrong Quiz State: {er}"

        match workflowResult with
        | Ok event ->
            let cmd =
                publishRunQuizEventCmd event.Quiz (CurrentQuestionChanged event)

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
                    publishRunQuizEventCmd event.Quiz (QuizzerParticipating event)

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
        let transformToRunQuizEvent event =
            match event with
            | RemoveQuizzer.Event.CurrentQuizzerChanged e -> RunQuizEvent.CurrentQuizzerChanged e
            | RemoveQuizzer.Event.QuizzerNoLongerParticipating e -> RunQuizEvent.QuizzerNoLongerParticipating e
            
        let result =
            RemoveQuizzer_Pipeline.removeQuizzer getQuiz saveQuiz withinQuizCommand
        
        match result with
        | Ok events ->
            let asyncBatch = (fun _ -> events
                                     |> List.map transformToRunQuizEvent
                                     |> List.map (publishQuizEvent (nameof hubStub.SendRunQuizEventOccurred) model.Code)
                                     |> Async.Parallel)
           
            let cmd = commandToRefresh asyncBatch
            model, cmd, None
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
        | Ok event -> model, (publishRunQuizEventCmd event.Quiz (CurrentQuizzerChanged event)), None
        | Result.Error error -> errorResult error

type private quizPage = Template<"wwwroot/Quiz.html">

let private teamView
    position
    ((teamModel, jumpOrder, currentQuizzer): TeamModel * string list * Option<Quizzer>)
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
                            currentQuizzer
                            |> Option.map (fun current ->
                                if quizzer.Name = current then
                                    "has-background-grey-lighter"
                                else
                                    "has-background-white-ter")
                            |> (fun current ->
                                match current with
                                | None -> ""
                                | Some q -> q)
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
        .TeamOne(
            teamView
                TeamPosition.TeamOne
                (model.TeamOne, model.JumpOrder, model.CurrentQuizzer)
                dispatch
        )
        .TeamTwo(
            teamView
                TeamPosition.TeamTwo
                (model.TeamTwo, model.JumpOrder, model.CurrentQuizzer)
                dispatch
        )
        .CurrentQuestion(string model.CurrentQuestion)
        .NextQuestion(fun _ -> dispatch (ChangeCurrentQuestion(model.CurrentQuestion + 1)))
        .UndoQuestion(fun _ -> dispatch (ChangeCurrentQuestion(Math.Max(model.CurrentQuestion - 1, 1))))
        .CurrentQuizzer(
            match model.CurrentQuizzer with
            | Some q -> $"{q}'s Turn"
            | None -> ""
        )
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
