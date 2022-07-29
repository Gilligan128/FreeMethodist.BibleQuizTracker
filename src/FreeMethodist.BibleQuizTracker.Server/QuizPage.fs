﻿module FreeMethodist.BibleQuizTracker.Server.QuizPage

open System
open System.Collections.ObjectModel
open System.ComponentModel
open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.AddQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow
open FreeMethodist.BibleQuizTracker.Server.Capabilities
open FreeMethodist.BibleQuizTracker.Server.Capabilities.Capabilities
open FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Workflow.ClearAppeal
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.FailAppeal.Workflow.FailAppeal
open FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow
open FreeMethodist.BibleQuizTracker.Server.RemoveQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.SelectQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.FailAppeal.Workflow
open FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Workflow
open FreeMethodist.BibleQuizTracker.Server.SelectQuizzer_Workflow
open Microsoft.AspNetCore.SignalR.Client
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open FreeMethodist.BibleQuizTracker.Server.ChangeCurrentQuestion_Pipeline
open FreeMethodist.BibleQuizTracker.Server.Capabilities

type ConnectionStatus =
    | Connected
    | Disconnected of DateTimeOffset
    | Unknown

type AnswerState =
    | DidNotAnswer
    | AnsweredCorrectly
    | AnsweredIncorrectly

type AppealState =
    | AppealFailure
    | NoFailure

type QuizzerModel =
    { Name: string
      Score: int
      ConnectionStatus: ConnectionStatus
      AnswerState: AnswerState
      AppealState: AppealState }

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

type LoadedModel =
    { JoiningQuizzer: string
      Code: QuizCode
      TeamOne: TeamModel
      TeamTwo: TeamModel
      JumpOrder: string list
      CurrentQuestion: int
      CurrentUser: User
      JumpState: JumpState
      AddQuizzer: AddQuizzerModel
      CurrentQuizzer: Quizzer option
      QuestionScores: Map<Quizzer, AnswerState * AppealState> list }


type Model =
    | NotYetLoaded of QuizCode
    | Loading of QuizCode
    | Loaded of LoadedModel

type PublishEventError =
    | FormError of string
    | RemoteError of exn

type AddQuizzerMessage =
    | Start
    | Cancel
    | Submit of AsyncOperationStatus<unit, Quiz>
    | SetName of string
    | SetTeam of TeamPosition

type WorkflowError<'a> =
    | Workflow of 'a
    | DbError of DbError

type WorkflowResult<'a> = Result<Quiz, WorkflowError<'a>>

type ChangeQuestionError =
    | FormError of string
    | QuizError of ChangeCurrentQuestion.Error

type Message =
    | InitializeQuizAndConnections of AsyncOperationStatus<QuizCode option, Result<Quiz, DbError>>
    | OnQuizEvent of AsyncOperationStatus<unit, Quiz>
    | ChangeCurrentQuestion of AsyncOperationStatus<int, WorkflowResult<ChangeQuestionError>>
    | WorkflowError of PublishEventError
    | AddQuizzer of AddQuizzerMessage
    | RemoveQuizzer of AsyncOperationStatus<Quizzer * TeamPosition, Quiz>
    | SelectQuizzer of AsyncOperationStatus<Quizzer, Quiz>
    | AnswerCorrectly of AsyncOperationStatus<unit, WorkflowResult<AnswerCorrectly.Error>>
    | AnswerIncorrectly of AsyncOperationStatus<unit, Quiz>
    | FailAppeal of AsyncOperationStatus<unit, Quiz>
    | ClearAppeal of AsyncOperationStatus<unit, Quiz>
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
      CurrentQuizzer = None
      QuestionScores = [] }

let tee sideEffect =
    fun x ->
        do sideEffect x
        x

let private refreshModel (quiz: Quiz) =
    let getAnswerState quizAnswer (quizzerState: QuizzerState) =
        let quizzerWasIncorrect =
            List.contains quizzerState.Name

        match quizAnswer with
        | Incomplete incorrectAnswerers when incorrectAnswerers |> quizzerWasIncorrect -> AnsweredIncorrectly
        | Incomplete _ -> DidNotAnswer
        | Complete (Answered question) when question.Answerer = quizzerState.Name -> AnsweredCorrectly
        | Complete (Answered question) when question.IncorrectAnswerers |> quizzerWasIncorrect -> AnsweredIncorrectly
        | Complete (Answered _) -> DidNotAnswer
        | Complete (Unanswered question) when question |> quizzerWasIncorrect -> AnsweredIncorrectly
        | Complete (Unanswered _) -> DidNotAnswer

    let getAppealState appeal quizzer =
        match appeal with
        | None -> NoFailure
        | Some appealer when appealer = quizzer -> AppealFailure
        | Some _ -> NoFailure

    let refreshQuizzer (currentQuestion: QuestionState) (quizzer: QuizzerState) =
        { Name = quizzer.Name
          Score = TeamScore.value quizzer.Score
          ConnectionStatus = Unknown
          AnswerState =
            quizzer
            |> getAnswerState currentQuestion.AnswerState
          AppealState =
            quizzer.Name
            |> getAppealState currentQuestion.FailedAppeal }

    let refreshTeam currentQuestion (team: QuizTeamState) =
        { Name = team.Name
          Score = TeamScore.value team.Score
          Quizzers =
            team.Quizzers
            |> List.map (refreshQuizzer currentQuestion) }

    let refreshQuestionScore (question: QuestionState) =
        let incorrectAnswer quizzer = (quizzer, AnsweredIncorrectly)

        let toMap questionScores =
            questionScores
            |> List.fold (fun map (q, answerState) -> map |> Map.add q answerState) Map.empty

        let insertAppealState answersWithNoAppealsYet =
            match question.FailedAppeal with
            | None -> answersWithNoAppealsYet
            | Some q ->
                answersWithNoAppealsYet
                |> Map.change q (fun questionOption ->
                    questionOption
                    |> Option.defaultValue (DidNotAnswer, AppealFailure)
                    |> fun (answer, _) -> Some(answer, AppealFailure))

        let answers =
            match question.AnswerState with
            | Incomplete quizzers -> quizzers |> List.map incorrectAnswer |> toMap
            | Complete (Answered question) ->
                [ (question.Answerer, AnsweredCorrectly) ]
                @ (question.IncorrectAnswerers
                   |> List.map incorrectAnswer)
                |> toMap
            | Complete (Unanswered question) -> question |> List.map incorrectAnswer |> toMap

        answers
        |> Map.map (fun key answer -> answer, NoFailure)
        |> insertAppealState


    let sortedList questionMap =
        questionMap
        |> Map.keys
        |> Seq.sortBy (fun k -> k |> PositiveNumber.value)
        |> Seq.map (fun k -> questionMap[k])
        |> Seq.toList

    match quiz with
    | Running runningQuiz ->
        let currentQuestion =
            runningQuiz.Questions.TryFind(runningQuiz.CurrentQuestion)
            |> Option.defaultValue QuestionState.initial

        { emptyModel with
            Code = runningQuiz.Code
            TeamOne = runningQuiz.TeamOne |> refreshTeam currentQuestion
            TeamTwo = runningQuiz.TeamTwo |> refreshTeam currentQuestion
            CurrentQuestion = PositiveNumber.value runningQuiz.CurrentQuestion
            CurrentQuizzer = runningQuiz.CurrentQuizzer
            CurrentUser = Quizmaster
            JoiningQuizzer = ""
            JumpOrder = [ "Jim"; "Juni"; "John" ]
            JumpState = Unlocked
            QuestionScores =
                runningQuiz.Questions
                |> sortedList
                |> List.map refreshQuestionScore }
    | Quiz.Completed _
    | Official _
    | Unvalidated _ -> emptyModel

let init quizCode previousQuizCode =
    NotYetLoaded quizCode, Cmd.ofMsg (InitializeQuizAndConnections(Started previousQuizCode))

let private hubStub =
    Unchecked.defaultof<QuizHub.Hub>

let workflowFormError =
    PublishEventError.FormError >> WorkflowError

let getAvailableCapabilities (capabilityProvider: RunQuizCapabilityProvider) user currentQuizzerOpt =
    let addQuizzer =
        capabilityProvider.AddQuizzer user

    let removeQuizzer =
        capabilityProvider.RemoveQuizzer user

    let answerCorrectly =
        capabilityProvider.AnswerCorrectly user currentQuizzerOpt

    let answerIncorrectly =
        capabilityProvider.AnswerIncorrectly user currentQuizzerOpt

    let failAppeal =
        capabilityProvider.FailAppeal user currentQuizzerOpt

    let clearAppeal =
        capabilityProvider.ClearAppeal user currentQuizzerOpt

    let selectQuizzer =
        capabilityProvider.SelectQuizzer user

    let changeCurrentQuestion =
        capabilityProvider.ChangeCurrentQuestion user

    addQuizzer,
    removeQuizzer,
    answerCorrectly,
    answerIncorrectly,
    failAppeal,
    clearAppeal,
    selectQuizzer,
    changeCurrentQuestion



let private updateLoaded
    (publishQuizEvent: PublishQuizEventTask)
    (getQuizAsync: GetQuiz)
    saveQuizAsync
    capabilityProvider
    msg
    model
    =
    let updateResultWithExternalError error =
        model, Cmd.none, ExternalMessage.Error error |> Some

    let mapWorkflowErrors mapWorkflowSpecificError error =
        match error with
        | WorkflowError.DbError error -> error |> mapDbErrorToString
        | WorkflowError.Workflow workflowError -> workflowError |> mapWorkflowSpecificError


    let publishRunQuizEvent quiz (event: RunQuizEvent) =
        publishQuizEvent (nameof hubStub.SendRunQuizEventOccurred) quiz event

    let publishEvents events =
        events
        |> List.map (publishRunQuizEvent model.Code)
        |> Async.Parallel
        |> Async.Ignore

    let mapExceptionToPublishEventError =
        (fun exn -> exn |> RemoteError |> WorkflowError)

    let workflowCmdList workflow (mapToQuizEvent: 'a -> RunQuizEvent) mapResult =

        let workflowX = fun _ -> workflow ()

        let mapResultWrapped mapResult workflowResult =
            match workflowResult with
            | Ok result -> result |> Result.Ok |> mapResult
            | Result.Error (WorkflowError.Workflow error) -> error |> Result.Error |> mapResult
            | Result.Error (WorkflowError.DbError dbError) -> dbError |> mapDbErrorToString |> workflowFormError

        let workflowWithSideEffects x =
            workflowX x
            |> AsyncResult.map (List.map mapToQuizEvent)
            |> AsyncResult.bind (publishEvents >> AsyncResult.ofAsync)
            |> AsyncResult.mapError WorkflowError.Workflow
            |> AsyncResult.bind (fun _ ->
                model.Code
                |> getQuizAsync
                |> AsyncResult.mapError WorkflowError.DbError)

        let workflowWithMappedResult x =
            workflowWithSideEffects x
            |> Async.map (fun result -> result |> mapResultWrapped mapResult)

        Cmd.OfAsync.either workflowWithMappedResult () id mapExceptionToPublishEventError

    let workflowCmdSingle workflow mapToQuizEvent mapResult =
        let newWorkflow =
            fun _ -> workflow () |> AsyncResult.map List.singleton

        workflowCmdList newWorkflow mapToQuizEvent mapResult

    let createQuizStateWorkflowError _ =
        "Quiz is not running" |> workflowFormError

    let mapOptionalCommand cmdOpt =
        match cmdOpt with
        | None -> model, Cmd.none, None
        | Some cmd -> model, cmd, None

    let runWorkflowEventsAsync events =
        asyncResult {
            let! runQuizEvents = events

            do!
                runQuizEvents
                |> publishEvents
                |> AsyncResult.ofAsync
        }
        |> AsyncResult.mapError WorkflowError.Workflow

    let reloadQuizAsync asyncTask =
        asyncResult {
            do! asyncTask

            let! quiz =
                model.Code
                |> getQuizAsync
                |> AsyncResult.mapError WorkflowError.DbError

            return quiz
        }

    let mapToAsyncOperationCmd asyncOperation resultAsync =
        resultAsync
        |> Async.map (fun result -> asyncOperation (Finished result))
        |> Cmd.OfAsync.result

    let refreshQuizOrError mapWorkflowSpecificErrors result =
        match result with
        | Ok quiz -> refreshModel quiz, Cmd.none, None
        | Result.Error (error) ->
            error
            |> mapWorkflowErrors mapWorkflowSpecificErrors
            |> fun errorString -> errorString |> updateResultWithExternalError

    let (addQuizzer,
         removeQuizzer,
         answerCorrectly,
         answerIncorrectly,
         failAppeal,
         clearAppeal,
         selectQuizzer,
         changeCurrentQuestion) =
        getAvailableCapabilities capabilityProvider model.CurrentUser model.CurrentQuizzer

    match msg with
    | DoNothing
    | InitializeQuizAndConnections (Started _)
    | InitializeQuizAndConnections (Finished _) -> model, Cmd.none, None
    | OnQuizEvent (Started _) ->
        let getQuizToRefresh =
            getQuizAsync model.Code
            |> Async.map (fun result ->
                match result with
                | Ok quiz -> quiz |> (Finished >> OnQuizEvent)
                | Result.Error error -> error |> mapDbErrorToString |> workflowFormError)

        model, Cmd.OfAsync.result getQuizToRefresh, None
    | OnQuizEvent (Finished quiz) -> refreshModel quiz, Cmd.none, None
    | ChangeCurrentQuestion (Started questionNumber) ->
        let mapToQuizEvent event =
            event |> RunQuizEvent.CurrentQuestionChanged

        let workflowResultOpt =
            changeCurrentQuestion
            |> Option.map (fun workflow ->
                asyncResult {
                    let! newQuestion =
                        questionNumber
                        |> PositiveNumber.create "QuestionNumber"
                        |> Result.mapError ChangeQuestionError.FormError
                        |> AsyncResult.ofResult

                    return!
                        workflow
                            { Quiz = model.Code
                              User = model.CurrentUser
                              Data = { Question = newQuestion } }
                        |> AsyncResult.mapError ChangeQuestionError.QuizError
                })

        workflowResultOpt
        |> Option.map (fun result ->
            result
            |> AsyncResult.map mapToQuizEvent
            |> AsyncResult.map List.singleton
            |> runWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd ChangeCurrentQuestion)
        |> mapOptionalCommand
    | ChangeCurrentQuestion (Finished result) ->
        let mapChangeQuestionError error =
            match error with
            | ChangeQuestionError.FormError er -> er
            | ChangeQuestionError.QuizError er -> $"Wrong Quiz State: {er}" in

        result
        |> refreshQuizOrError mapChangeQuestionError

    | Message.WorkflowError _ -> model, Cmd.none, None //handled elsewhere
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
    | AddQuizzer (Submit (Started _)) ->
        match model.AddQuizzer with
        | AddQuizzerModel.Active (name, team) ->
            let mapResult result =
                match result with
                | Ok quiz -> Message.AddQuizzer(Submit(Finished quiz))
                | Result.Error error ->
                    match error with
                    | AddQuizzer.Error.QuizState quizStateError -> $"Wrong Quiz State: {quizStateError}"
                    | AddQuizzer.Error.QuizzerAlreadyAdded quizzer -> $"Quizzer {quizzer} already added"
                    | AddQuizzer.DbError dbError -> dbError |> mapDbErrorToString
                    |> PublishEventError.FormError
                    |> Message.WorkflowError

            let mapQuizEvent event = event |> QuizzerParticipating

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
    | AddQuizzer (Submit (Finished quiz)) -> refreshModel quiz, Cmd.none, None
    | RemoveQuizzer (Started (name, teamPosition)) ->
        let withinQuizCommand: RemoveQuizzer.Command =
            { Quiz = model.Code
              Data = { Quizzer = name; Team = teamPosition }
              User = Quizmaster }

        let transformToRunQuizEvent event =
            match event with
            | RemoveQuizzer.Event.CurrentQuizzerChanged e -> RunQuizEvent.CurrentQuizzerChanged e
            | RemoveQuizzer.Event.QuizzerNoLongerParticipating e -> RunQuizEvent.QuizzerNoLongerParticipating e

        let mapResultToMessage result =
            match result with
            | Ok quiz -> Message.RemoveQuizzer(Finished quiz)
            | Result.Error (RemoveQuizzer.QuizStateError quizStateError) ->
                PublishEventError.FormError $"Wrong Quiz State: {quizStateError}"
                |> WorkflowError
            | Result.Error (RemoveQuizzer.QuizzerNotParticipating quizzer) ->
                PublishEventError.FormError $"Quizzer {quizzer} is already not participating"
                |> WorkflowError
            | Result.Error (RemoveQuizzer.DbError dbError) -> dbError |> mapDbErrorToString |> workflowFormError

        let workflow =
            fun () -> RemoveQuizzer_Pipeline.removeQuizzer getQuizAsync saveQuizAsync withinQuizCommand

        model, (workflowCmdList workflow transformToRunQuizEvent mapResultToMessage), None
    | RemoveQuizzer (Finished quiz) -> refreshModel quiz, Cmd.none, None
    | SelectQuizzer (Started name) ->
        let mapEvent event = CurrentQuizzerChanged event

        let mapResultToMessage result =
            match result with
            | Ok quiz -> Message.SelectQuizzer(Finished quiz)
            | Result.Error (SelectQuizzer.Error.QuizState quizStateError) -> createQuizStateWorkflowError quizStateError
            | Result.Error (SelectQuizzer.Error.QuizzerAlreadyCurrent) -> Message.DoNothing
            | Result.Error (SelectQuizzer.Error.DbError dbError) -> dbError |> mapDbErrorToString |> workflowFormError
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
    | SelectQuizzer (Finished quiz) -> refreshModel quiz, Cmd.none, None
    | AnswerCorrectly (Started _) ->
        let mapEvent event =
            match event with
            | AnswerCorrectly.CurrentQuestionChanged e -> CurrentQuestionChanged e
            | AnswerCorrectly.IndividualScoreChanged e -> IndividualScoreChanged e
            | AnswerCorrectly.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        let workflowOpt =
            answerCorrectly
            |> Option.map (fun workflow ->
                workflow
                    { Data = ()
                      Quiz = model.Code
                      User = model.CurrentUser })


        workflowOpt
        |> Option.map (fun result ->
            result
            |> AsyncResult.map (List.map mapEvent)
            |> runWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd AnswerCorrectly)
        |> mapOptionalCommand
    | AnswerCorrectly (Finished result) ->
        let mapWorkflowSpecificErrors workflowError =
            match workflowError with
            | (AnswerCorrectly.Error.DuplicateQuizzer er) -> $"There is more than one quizzer with name {er}"
            | (AnswerCorrectly.QuizzerNotFound er) -> $"Quizzer {er} was not found in this quiz"
            | (AnswerCorrectly.Error.QuizStateError _) -> "Quiz is not running"
            | (AnswerCorrectly.Error.NoCurrentQuizzer) -> "No one has jumped yet"
            | (AnswerCorrectly.Error.QuizzerAlreadyAnsweredCorrectly (QuizAnswer.QuizzerAlreadyAnsweredCorrectly (quizzer,
                                                                                                                  question))) ->
                $"Quizzer {quizzer} already correctly answered question {question |> PositiveNumber.value}"
            | (AnswerCorrectly.Error.DbError dbError) -> dbError |> mapDbErrorToString

        match result with
        | Ok quiz -> refreshModel quiz, Cmd.none, None
        | Result.Error (error) ->
            error
            |> mapWorkflowErrors mapWorkflowSpecificErrors
            |> fun errorString -> errorString |> updateResultWithExternalError


    | AnswerIncorrectly (Started _) ->
        let workflow =
            fun () ->
                AnswerIncorrectly.Pipeline.answerIncorrectly
                    getQuizAsync
                    saveQuizAsync
                    { Quiz = model.Code
                      User = model.CurrentUser
                      Data = () }

        let mapEvent event =
            match event with
            | AnswerIncorrectly.Event.CurrentQuizzerChanged e -> RunQuizEvent.CurrentQuizzerChanged e
            | AnswerIncorrectly.Event.IndividualScoreChanged e -> RunQuizEvent.IndividualScoreChanged e
            | AnswerIncorrectly.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        let mapResult result =
            match result with
            | Ok quiz -> AnswerIncorrectly(Finished quiz)
            | Result.Error (AnswerIncorrectly.QuizState quizState) -> createQuizStateWorkflowError quizState
            | Result.Error (AnswerIncorrectly.NoCurrentQuizzer _) -> "No current Quizzer" |> workflowFormError
            | Result.Error (AnswerIncorrectly.QuizzerAlreadyAnsweredIncorrectly (QuizAnswer.QuizzerAlreadyAnsweredIncorrectly (quizzer,
                                                                                                                               questionNumber))) ->
                $"Quizzer {quizzer} already answered question {questionNumber |> PositiveNumber.value} incorrectly"
                |> workflowFormError
            | Result.Error (AnswerIncorrectly.Error.DbError dbError) ->
                dbError |> mapDbErrorToString |> workflowFormError

        let cmd =
            workflowCmdList workflow mapEvent mapResult

        model, cmd, None
    | AnswerIncorrectly (Finished quiz) -> refreshModel quiz, Cmd.none, None
    | FailAppeal (Started _) ->
        let workflow =
            fun _ ->
                { Quiz = model.Code
                  Data = ()
                  User = model.CurrentUser }
                |> FailAppeal.Pipeline.failAppeal getQuizAsync saveQuizAsync

        let mapEvent event =
            match event with
            | FailAppeal.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        let mapResult result =
            match result with
            | Ok quiz -> quiz |> Finished |> FailAppeal
            | Result.Error (FailAppeal.Error.QuizState _) -> "Wrong Quiz state" |> workflowFormError
            | Result.Error (FailAppeal.Error.AppealAlreadyFailed _) -> "Appeal already failed" |> workflowFormError
            | Result.Error (FailAppeal.Error.NoCurrentQuizzer _) -> "No current quizzer" |> workflowFormError
            | Result.Error (FailAppeal.Error.DbError error) -> error |> mapDbErrorToString |> workflowFormError

        let cmd =
            workflowCmdList workflow mapEvent mapResult

        model, cmd, None
    | FailAppeal (Finished quiz) -> refreshModel quiz, Cmd.none, None
    | ClearAppeal (Started _) ->
        let workflow =
            fun _ ->
                { Quiz = model.Code
                  Data = ()
                  User = model.CurrentUser }
                |> ClearAppeal.Pipeline.clearAppeal getQuizAsync saveQuizAsync

        let mapEvent event =
            match event with
            | ClearAppeal.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        let mapResult result =
            match result with
            | Ok quiz -> quiz |> Finished |> ClearAppeal
            | Result.Error (ClearAppeal.Error.QuizState _) -> "Wrong Quiz state" |> workflowFormError
            | Result.Error (ClearAppeal.Error.NoFailedAppeal _) ->
                "There is no failed appeal to clear"
                |> workflowFormError
            | Result.Error (ClearAppeal.Error.DbError dbError) -> dbError |> mapDbErrorToString |> workflowFormError

        let cmd =
            workflowCmdList workflow mapEvent mapResult

        model, cmd, None
    | ClearAppeal (Finished quiz) -> refreshModel quiz, Cmd.none, None

let update
    connectToQuizEvents
    onQuizEvent
    (publishQuizEvent: PublishQuizEventTask)
    (getQuizAsync: GetQuiz)
    saveQuizAsync
    capabilityProvider
    msg
    model
    =
    match model, msg with
    | Loading code, Message.InitializeQuizAndConnections (Finished result) ->
        match result with
        | Ok quiz -> Loaded(refreshModel quiz), Cmd.none, None
        | Result.Error error ->
            let externalMessage =
                error |> mapDbErrorToString

            NotYetLoaded code, Cmd.none, ExternalMessage.Error externalMessage |> Some
    | NotYetLoaded code, Message.InitializeQuizAndConnections (Started previousQuizCode) ->
        let loadAndConnectToQuizCmd =
            asyncResult {
                do!
                    connectToQuizEvents code previousQuizCode
                    |> AsyncResult.ofAsync

                let! quiz = getQuizAsync code

                return quiz
            }
            |> Async.timeoutNone 3000
            |> Async.map (fun task ->
                task
                |> Option.defaultValue (Result.Error(DbError.RemoteError "Loading the quiz timed out")))
            |> Async.map (fun result ->
                result
                |> (Message.InitializeQuizAndConnections << Finished))
            |> Cmd.OfAsync.result

        let listenToEventsCmd =
            (fun dispatch ->
                let refreshQuizOnEvent _ =
                    getQuizAsync code
                    |> AsyncResult.map (fun quiz -> dispatch (Message.OnQuizEvent(Finished quiz)))
                    |> Async.Ignore
                    |> Async.StartImmediate //RunSynchronously did not work here

                (onQuizEvent refreshQuizOnEvent) |> ignore)
            |> Cmd.ofSub

        Loading code,
        Cmd.batch [ loadAndConnectToQuizCmd
                    listenToEventsCmd ],
        None
    | _, Message.WorkflowError error ->
        let errorMessage =
            match error with
            | PublishEventError.FormError er -> er
            | RemoteError exn -> exn.Message

        model, Cmd.none, errorMessage |> ExternalMessage.Error |> Some
    | Loaded loadedModel, _ ->
        let loaded, cmd, externalMsg =
            updateLoaded publishQuizEvent getQuizAsync saveQuizAsync capabilityProvider msg loadedModel

        Loaded loaded, cmd, externalMsg
    | Loading _, _
    | NotYetLoaded _, _ -> model, Cmd.none, None


type private quizPage = Template<"wwwroot/Quiz.html">

type ItemizedScoreModel =
    { TeamOne: TeamModel
      TeamTwo: TeamModel
      Questions: Map<Quizzer, AnswerState * AppealState> list }

type private itemizedPage = Template<"wwwroot/ItemizedScore.html">

let private itemizedScoreView model dispatch =
    let answerScore answerState =
        let score =
            TeamScore.initial
            |> (TeamScore.correctAnswer)
            |> TeamScore.value

        match answerState with
        | AnsweredCorrectly -> score
        | AnsweredIncorrectly -> 0
        | DidNotAnswer -> 0

    let appealScore appealState =
        let score =
            TeamScore.initial
            |> TeamScore.failAppeal
            |> TeamScore.value

        match appealState with
        | NoFailure -> 0
        | AppealFailure -> score

    let quizzerScore questionState =
        questionState
        |> Option.map (fun (answer, appeal) -> (answerScore answer), (appealScore appeal))
        |> Option.defaultValue (0, 0)

    let scoreList questions questionNumber quizzer =
        (questions
         |> List.take (questionNumber)
         |> List.map (fun qs -> qs |> Map.tryFind (quizzer))
         |> List.map quizzerScore)

    let quizzerRunningScore questions questionNumber quizzer =
        scoreList questions questionNumber quizzer
        |> List.map fst //appeals only score at the team level
        |> List.sum

    let eventOccurred (answer, appeal) =
        match answer, appeal with
        | AnsweredCorrectly, _ -> true
        | _, AppealFailure -> true
        | AnsweredIncorrectly, NoFailure -> false
        | DidNotAnswer, NoFailure -> false

    let teamEventOccurred team question =
        team.Quizzers
        |> List.map (fun qz -> question |> Map.tryFind (qz.Name))
        |> List.exists (fun q ->
            q
            |> (Option.defaultValue (AnsweredIncorrectly, NoFailure))
            |> eventOccurred)

    let teamRunningScore questions questionNumber team =
        team.Quizzers
        |> List.fold
            (fun state qz ->
                let int32s =
                    scoreList questions questionNumber qz.Name
                    |> List.map (fun (f, s) -> f + s)

                state
                + (int32s //appeals are scored at team level
                   |> List.sum))
            0

    let findQuestionQuizzerState question (quizzer: QuizzerModel) = question |> Map.tryFind quizzer.Name

    let formatScore score =
        match score with
        | 0 -> "-"
        | number -> $"{number}"

    let showAppeal questionQuizzerState =
        match questionQuizzerState with
        | None -> "is-hidden"
        | Some (_, NoFailure) -> "is-hidden"
        | Some (_, AppealFailure) -> ""

    let quizzerView question quizzer =
        itemizedPage
            .Quizzer()
            .AppealVisible(
                quizzer
                |> findQuestionQuizzerState question
                |> showAppeal
            )
            .Score(
                quizzer
                |> findQuestionQuizzerState question
                |> quizzerScore
                |> fst
                |> formatScore
            )
            .Elt()

    itemizedPage()
        .TeamOneName(model.TeamOne.Name)
        .TeamOneColspan((model.TeamOne.Quizzers |> List.length) + 1)
        .TeamOneHeading(
            concat {
                for quizzer in model.TeamOne.Quizzers do
                    th { quizzer.Name }
            }
        )
        .TeamTwoName(model.TeamTwo.Name)
        .TeamTwoColspan((model.TeamTwo.Quizzers |> List.length) + 1)
        .TeamTwoHeading(
            concat {
                for quizzer in model.TeamTwo.Quizzers do
                    th { quizzer.Name }
            }
        )
        .Questions(
            concat {
                for (number, question) in model.Questions |> List.indexed do
                    itemizedPage
                        .Question()
                        .Number(number + 1 |> string)
                        .TeamOneScore(
                            if teamEventOccurred model.TeamOne question then
                                teamRunningScore model.Questions (number + 1) model.TeamOne
                                |> formatScore
                            else
                                "-"
                        )
                        .TeamOneQuizzers(
                            concat {
                                for quizzer in model.TeamOne.Quizzers do
                                    quizzerView question quizzer
                            }
                        )
                        .TeamTwoScore(
                            if teamEventOccurred model.TeamTwo question then
                                teamRunningScore model.Questions (number + 1) model.TeamTwo
                                |> formatScore
                            else
                                "-"
                        )
                        .TeamTwoQuizzers(
                            concat {
                                for quizzer in model.TeamTwo.Quizzers do
                                    quizzerView question quizzer
                            }
                        )
                        .Elt()
            }
        )
        .TeamOneFooter(
            concat {
                for quizzer in model.TeamOne.Quizzers do
                    td {
                        quizzerRunningScore model.Questions (model.Questions |> List.length) quizzer.Name
                        |> formatScore
                    }
            }
        )
        .TeamOneScore(model.TeamOne.Score |> string)
        .TeamTwoFooter(
            concat {
                for quizzer in model.TeamTwo.Quizzers do
                    td {
                        quizzerRunningScore model.Questions (model.Questions |> List.length) quizzer.Name
                        |> formatScore
                    }
            }
        )
        .TeamTwoScore(model.TeamTwo.Score |> string)
        .Elt()

let private teamView
    position
    ((teamModel, jumpOrder, currentQuizzer): TeamModel * string list * Option<Quizzer>)
    (dispatch: Dispatch<Message>)
    =
    quizPage
        .Team()
        .Name(teamModel.Name)
        .TeamColor(
            match position with
            | TeamOne -> "success"
            | TeamTwo -> "danger"
        )
        .ScoreReadOnly(string teamModel.Score)
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
                        .Remove(fun _ -> dispatch (RemoveQuizzer(Started(quizzer.Name, position))))
                        .Select(fun _ -> dispatch (SelectQuizzer(Started quizzer.Name)))
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
                        .AnswerColor(
                            match quizzer.AnswerState with
                            | DidNotAnswer -> ""
                            | AnsweredCorrectly -> "success"
                            | AnsweredIncorrectly -> "danger"
                        )
                        .AppealVisible(
                            match quizzer.AppealState with
                            | NoFailure -> "is-hidden"
                            | AppealFailure -> ""
                        )
                        .Elt()
            }
        )
        .Elt()

let page linkToQuiz (model: Model) (dispatch: Dispatch<Message>) =
    let isTeam model teamOneValue teamTwoValue =
        match model.AddQuizzer with
        | Inert -> false
        | Active (_, TeamOne) -> teamOneValue
        | Active (_, TeamTwo) -> teamTwoValue

    match model with
    | NotYetLoaded code -> p { $"Quiz {code} has not yet been loaded" }
    | Loading code -> p { $"Quiz {code} is loading..." }
    | Loaded model ->
        quizPage()
            .QuizCode(model.Code)
            .QuizUrl(linkToQuiz <| model.Code)
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
            .NextQuestion(fun _ -> dispatch (ChangeCurrentQuestion(Started(model.CurrentQuestion + 1))))
            .UndoQuestion(fun _ -> dispatch (ChangeCurrentQuestion(Started(Math.Max(model.CurrentQuestion - 1, 1)))))
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
            .AddQuizzerIsTeamOne(isTeam model true false)
            .AddQuizzerIsTeamTwo(isTeam model false true)
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
            .AddQuizzerSubmit(fun _ -> dispatch (AddQuizzer(Submit(Started()))))
            .SetAddQuizzerTeamOne(fun _ -> dispatch (AddQuizzer(SetTeam TeamOne)))
            .SetAddQuizzerTeamTwo(fun _ -> dispatch (AddQuizzer(SetTeam TeamTwo)))
            .AnswerCorrectly(fun _ -> dispatch (AnswerCorrectly(Started())))
            .AnswerIncorrectly(fun _ -> dispatch (AnswerIncorrectly(Started())))
            .FailAppeal(fun _ -> dispatch (FailAppeal(Started())))
            .ClearAppeal(fun _ -> dispatch (ClearAppeal(Started())))
            .ItemizedScore(
                itemizedScoreView
                    { TeamOne = model.TeamOne
                      TeamTwo = model.TeamTwo
                      Questions = model.QuestionScores }
                    dispatch
            )
            .Elt()
