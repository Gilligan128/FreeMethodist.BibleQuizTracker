module FreeMethodist.BibleQuizTracker.Server.QuizPage

open System
open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.AddQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Workflow
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Pipeline
open FreeMethodist.BibleQuizTracker.Server.RemoveQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.SelectQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.AspNetCore.SignalR.Client
open Microsoft.FSharp.Core
open FreeMethodist.BibleQuizTracker.Server.ChangeCurrentQuestion_Pipeline


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
      Code: QuizCode
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
    | InitializeQuizAndConnections of AsyncOperationStatus<QuizCode option, Quiz>
    | OverrideScore of int * TeamPosition
    | RefreshQuiz of AsyncOperationStatus<unit, Quiz>
    | ChangeCurrentQuestion of int
    | WorkflowError of PublishEventError
    | AddQuizzer of AddQuizzerMessage
    | RemoveQuizzer of Quizzer * TeamPosition
    | SelectQuizzer of Quizzer
    | AnswerCorrectly
    | DoNothing


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
    { emptyModel with Code = quizCode }, Cmd.ofMsg (InitializeQuizAndConnections (Started previousQuizCode))

type OverrideScoreErrors =
    | DomainError of QuizStateError
    | FormError of string

type ChangeQuestionError =
    | FormError of string
    | QuizError of QuizStateError

let private overrideScoreAsync getQuiz saveQuiz (model: Model) (score: int) (team: TeamPosition) =
    asyncResult {
        let! newScore =
            TeamScore.create score
            |> Result.mapError (OverrideScoreErrors.FormError)
            |> AsyncResult.ofResult

        let command =
            { Quiz = model.Code
              Data = { Team = team; NewScore = newScore }
              User = Quizmaster }

        return!
            overrideTeamScoreAsync getQuiz saveQuiz command
            |> AsyncResult.mapError (DomainError)
    }

let private hubStub =
    Unchecked.defaultof<QuizHub.Hub>

let update
    connectToQuizEvents
    onQuizEvent
    (publishQuizEvent: PublishQuizEventTask)
    getQuizAsync
    saveQuizAsync
    msg
    model
    =

    let publishRunQuizEvent quiz event =
        publishQuizEvent (nameof hubStub.SendRunQuizEventOccurred) quiz event

    let mapExceptionToPublishEventError =
        (fun exn -> exn |> RemoteError |> WorkflowError)

    let workflowCmdList workflow mapToQuizEvent mapResult =
        let publishEvents events =
            events
            |> List.map mapToQuizEvent
            |> List.map (fun (event, code) -> publishRunQuizEvent code event)
            |> Async.Parallel

        let workflowX = fun _ -> workflow ()

        let workflowWithSideEffects x =
            workflowX x
            |> AsyncResult.bind (publishEvents >> AsyncResult.ofAsync)
            |> AsyncResult.bind (fun _ ->
                (model.Code
                 |> (getQuizAsync >> AsyncResult.ofAsync)))

        Cmd.OfAsync.either workflowWithSideEffects () mapResult mapExceptionToPublishEventError

    let workflowCmdSingle workflow mapToQuizEvent mapResult =
        let newWorkflow =
            fun () -> workflow () |> AsyncResult.map List.singleton

        workflowCmdList newWorkflow mapToQuizEvent mapResult

    let workflowFormError =
        PublishEventError.FormError >> WorkflowError

    match msg with
    | DoNothing -> model, Cmd.none, None
    | OverrideScore (score, teamPosition) ->
        let mapResultToMessage result =
            match result with
            | Ok quiz -> Message.RefreshQuiz(Finished quiz)
            | Result.Error error ->
                match error with
                | OverrideScoreErrors.DomainError error ->
                    PublishEventError.FormError $"Quiz is in the wrong state: {error}"
                    |> WorkflowError
                | OverrideScoreErrors.FormError error -> PublishEventError.FormError error |> WorkflowError

        let mapEvent event =
            RunQuizEvent.TeamScoreChanged event, event.Quiz

        let workflow =
            fun () -> overrideScoreAsync getQuizAsync saveQuizAsync model score teamPosition

        let cmd =
            workflowCmdSingle workflow mapEvent mapResultToMessage

        model, cmd, None
    | InitializeQuizAndConnections (Started previousQuizCode) ->
        let loadAndConnectToQuizCmd =
            async {
                do! connectToQuizEvents model.Code previousQuizCode
                let! quiz = getQuizAsync model.Code
                return Message.RefreshQuiz(Finished quiz)
            }
            |> Cmd.OfAsync.result

        let listenToEventsCmd =
            (fun dispatch ->
                let refreshQuizOnEvent _ =
                    getQuizAsync model.Code
                    |> Async.map (fun quiz -> dispatch (Message.RefreshQuiz(Finished quiz)))
                    |> Async.StartImmediate //Run synchronously did not work here

                onQuizEvent refreshQuizOnEvent |> ignore)
            |> Cmd.ofSub

        { emptyModel with Code = model.Code },
        Cmd.batch [ loadAndConnectToQuizCmd
                    listenToEventsCmd ],
        None
    | InitializeQuizAndConnections (Finished quiz) -> refreshModel quiz, Cmd.none, None
    | RefreshQuiz (Started _) ->
        let getQuizToRefresh =
            getQuizAsync model.Code
            |> Async.map (Finished >> RefreshQuiz)

        model, Cmd.OfAsync.result getQuizToRefresh, None
    | RefreshQuiz (Finished quiz) -> refreshModel quiz, Cmd.none, None
    | ChangeCurrentQuestion questionNumber ->
        let mapToQuizEvent event =
            event |> RunQuizEvent.CurrentQuestionChanged, event.Quiz

        let mapWorkflowResult result =
            let moveQuestionErrorMessage error =
                match error with
                | ChangeQuestionError.FormError er -> er
                | ChangeQuestionError.QuizError er -> $"Wrong Quiz State: {er}" in

            match result with
            | Ok quiz -> RefreshQuiz(Finished quiz)
            | Result.Error error ->
                error
                |> moveQuestionErrorMessage
                |> PublishEventError.FormError
                |> WorkflowError

        let workflow =
            fun () ->
                asyncResult {
                    let! questionNumber =
                        PositiveNumber.create questionNumber "QuestionNumber"
                        |> Result.mapError ChangeQuestionError.FormError
                        |> AsyncResult.ofResult

                    return!
                        changeCurrentQuestionAsync
                            getQuizAsync
                            saveQuizAsync
                            { Quiz = model.Code
                              User = Quizmaster
                              Data = { Question = questionNumber } }
                        |> AsyncResult.mapError ChangeQuestionError.QuizError
                }

        let cmd =
            workflowCmdSingle workflow mapToQuizEvent mapWorkflowResult

        model, cmd, None

    | Message.WorkflowError error ->
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
            let mapResult result =
                match result with
                | Ok quiz -> Message.RefreshQuiz(Finished quiz)
                | Result.Error error ->
                    match error with
                    | AddQuizzer.Error.QuizState quizStateError -> $"Wrong Quiz State: {quizStateError}"
                    | AddQuizzer.Error.QuizzerAlreadyAdded quizzer -> $"Quizzer {quizzer} already added"
                    |> PublishEventError.FormError
                    |> Message.WorkflowError

            let mapQuizEvent event =
                event |> QuizzerParticipating, event.Quiz

            let workflow =
                fun () ->
                    let inputCommand: AddQuizzer.Command =
                        { Quiz = model.Code
                          Data = { Name = name; Team = team }
                          User = model.CurrentUser }

                    AddQuizzer_Pipeline.addQuizzerAsync getQuizAsync saveQuizAsync inputCommand

            let cmd =
                workflowCmdSingle workflow mapQuizEvent mapResult

            { model with AddQuizzer = Inert }, cmd, None
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
            | RemoveQuizzer.Event.CurrentQuizzerChanged e -> RunQuizEvent.CurrentQuizzerChanged e, e.Quiz
            | RemoveQuizzer.Event.QuizzerNoLongerParticipating e -> RunQuizEvent.QuizzerNoLongerParticipating e, e.Quiz

        let mapResultToMessage result =
            match result with
            | Ok quiz -> Message.RefreshQuiz(Finished quiz)
            | Result.Error (RemoveQuizzer.QuizStateError quizStateError) ->
                PublishEventError.FormError $"Wrong Quiz State: {quizStateError}"
                |> WorkflowError
            | Result.Error (RemoveQuizzer.QuizzerNotParticipating quizzer) ->
                PublishEventError.FormError $"Quizzer {quizzer} is already not participating"
                |> WorkflowError

        let workflow =
            fun () -> RemoveQuizzer_Pipeline.removeQuizzer getQuizAsync saveQuizAsync withinQuizCommand

        model, (workflowCmdList workflow transformToRunQuizEvent mapResultToMessage), None

    | SelectQuizzer (name) ->
        let mapEvent event = CurrentQuizzerChanged event, event.Quiz

        let mapResultToMessage result =
            match result with
            | Ok quiz -> Message.RefreshQuiz(Finished quiz)
            | Result.Error (SelectQuizzer.Error.QuizState quizStateError) ->
                PublishEventError.FormError $"Wrong Quiz State: {quizStateError}"
                |> WorkflowError
            | Result.Error (SelectQuizzer.Error.QuizzerAlreadyCurrent) -> Message.DoNothing
            | Result.Error (SelectQuizzer.Error.QuizzerNotParticipating quizzer) ->
                PublishEventError.FormError $"Quizzer {quizzer} is not participating"
                |> WorkflowError

        let command: SelectQuizzer.Command =
            { Quiz = model.Code
              User = model.CurrentUser
              Data = { Quizzer = name } }

        let workflow =
            fun () -> SelectQuizzer_Pipeline.selectQuizzer getQuizAsync saveQuizAsync command

        model, (workflowCmdSingle workflow mapEvent mapResultToMessage), None
    | AnswerCorrectly ->
        let mapEvent event =
            match event with
            | AnswerCorrectly.CurrentQuestionChanged e -> CurrentQuestionChanged e, e.Quiz
            | AnswerCorrectly.IndividualScoreChanged e -> IndividualScoreChanged e, e.Quiz
            | AnswerCorrectly.TeamScoreChanged e -> TeamScoreChanged e, e.Quiz

        let workflow =
            fun () ->
                AnswerCorrectly_Pipeline.answerCorrectly
                    getQuizAsync
                    saveQuizAsync
                    { Data = ()
                      Quiz = model.Code
                      User = model.CurrentUser }

        let mapResult result =
            match result with
            | Ok quiz -> RefreshQuiz(Finished quiz)
            | Result.Error (AnswerCorrectly.Error.DuplicateQuizzer er) ->
                $"There is more than one quizzer with name {er}"
                |> workflowFormError
            | Result.Error (AnswerCorrectly.QuizzerNotFound er) ->
                $"Quizzer {er} was not found in this quiz"
                |> workflowFormError
            | Result.Error (AnswerCorrectly.Error.QuizStateError error) -> $"Quiz is not running" |> workflowFormError
            | Result.Error (AnswerCorrectly.Error.NoCurrentQuizzer) -> "No one has jumped yet" |> workflowFormError

        let cmd =
            workflowCmdList workflow mapEvent mapResult

        model, cmd, None



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
        .TeamOne(teamView TeamPosition.TeamOne (model.TeamOne, model.JumpOrder, model.CurrentQuizzer) dispatch)
        .TeamTwo(teamView TeamPosition.TeamTwo (model.TeamTwo, model.JumpOrder, model.CurrentQuizzer) dispatch)
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
