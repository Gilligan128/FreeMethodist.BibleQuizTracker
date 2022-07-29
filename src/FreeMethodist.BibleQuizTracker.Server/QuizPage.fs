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
    | NotYetLoaded of QuizCode*User
    | Loading of QuizCode*User
    | Loaded of LoadedModel

type PublishEventError =
    | FormError of string
    | RemoteError of exn

type WorkflowError<'a> =
    | Workflow of 'a
    | DbError of DbError

type WorkflowResult<'a> = Result<Quiz, WorkflowError<'a>>

type AddQuizzerMessage =
    | Start
    | Cancel
    | Submit of AsyncOperationStatus<unit, WorkflowResult<AddQuizzer.Error>>
    | SetName of string
    | SetTeam of TeamPosition

type ChangeQuestionError =
    | FormError of string
    | QuizError of ChangeCurrentQuestion.Error

type Message =
    | InitializeQuizAndConnections of AsyncOperationStatus<QuizCode option, Result<Quiz, DbError>>
    | OnQuizEvent of AsyncOperationStatus<unit, Quiz>
    | ChangeCurrentQuestion of AsyncOperationStatus<int, WorkflowResult<ChangeQuestionError>>
    | WorkflowError of PublishEventError
    | AddQuizzer of AddQuizzerMessage
    | RemoveQuizzer of AsyncOperationStatus<Quizzer * TeamPosition, WorkflowResult<RemoveQuizzer.Error>>
    | SelectQuizzer of AsyncOperationStatus<Quizzer, WorkflowResult<SelectQuizzer.Error>>
    | AnswerCorrectly of AsyncOperationStatus<unit, WorkflowResult<AnswerCorrectly.Error>>
    | AnswerIncorrectly of AsyncOperationStatus<unit, WorkflowResult<AnswerIncorrectly.Error>>
    | FailAppeal of AsyncOperationStatus<unit, WorkflowResult<FailAppeal.Error>>
    | ClearAppeal of AsyncOperationStatus<unit, WorkflowResult<ClearAppeal.Error>>
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

let private refreshModel (quiz: Quiz, user: User) =
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

    let stateMatchedModel =
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
    {stateMatchedModel with CurrentUser = user}

let init user quizCode previousQuizCode =
    NotYetLoaded (quizCode, user), Cmd.ofMsg (InitializeQuizAndConnections(Started previousQuizCode))

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
    capabilityProvider
    msg
    model
    =
    let refreshModel quiz =
        refreshModel (quiz,model.CurrentUser)
   
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

    let matchOptionalCommand cmdOpt =
        match cmdOpt with
        | None -> model, Cmd.none, None
        | Some cmd -> model, cmd, None

    let publishWorkflowEventsAsync events =
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
            |> publishWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd ChangeCurrentQuestion)
        |> matchOptionalCommand
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
            let mapQuizEvent event = event |> QuizzerParticipating

            let inputCommand: AddQuizzer.Command =
                { Quiz = model.Code
                  Data = { Name = name; Team = team }
                  User = model.CurrentUser }

            addQuizzer
            |> Option.map (fun workflow ->
                workflow inputCommand
                |> AsyncResult.map mapQuizEvent
                |> AsyncResult.map List.singleton
                |> publishWorkflowEventsAsync
                |> reloadQuizAsync
                |> mapToAsyncOperationCmd (AddQuizzerMessage.Submit >> Message.AddQuizzer))
            |> matchOptionalCommand
        | AddQuizzerModel.Inert ->
            model,
            Cmd.none,
            ExternalMessage.Error "How are you even submitting from an inert AddQuizzer state?"
            |> Some
    | AddQuizzer (Submit (Finished result)) ->
        let mapAddQuizzerError error =
            match error with
            | AddQuizzer.Error.QuizState quizStateError -> $"Wrong Quiz State: {quizStateError}"
            | AddQuizzer.Error.QuizzerAlreadyAdded quizzer -> $"Quizzer {quizzer} already added"
            | AddQuizzer.DbError dbError -> dbError |> mapDbErrorToString

        result
        |> function
            | Ok quiz ->
                quiz
                |> refreshModel
                |> fun model -> { model with AddQuizzer = Inert }, Cmd.none, None
            | Result.Error error ->
                let error =
                    error |> mapWorkflowErrors mapAddQuizzerError

                { model with AddQuizzer = Inert }, Cmd.none, error |> ExternalMessage.Error |> Some
    | RemoveQuizzer (Started (name, teamPosition)) ->
        let withinQuizCommand: RemoveQuizzer.Command =
            { Quiz = model.Code
              Data = { Quizzer = name; Team = teamPosition }
              User = Quizmaster }

        let transformToRunQuizEvent event =
            match event with
            | RemoveQuizzer.Event.CurrentQuizzerChanged e -> RunQuizEvent.CurrentQuizzerChanged e
            | RemoveQuizzer.Event.QuizzerNoLongerParticipating e -> RunQuizEvent.QuizzerNoLongerParticipating e

        removeQuizzer
        |> Option.map (fun workflow ->
            workflow withinQuizCommand
            |> AsyncResult.map (List.map transformToRunQuizEvent)
            |> publishWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd RemoveQuizzer)
        |> matchOptionalCommand
    | RemoveQuizzer (Finished result) ->
        let mapRemoveError error =
            match error with
            | RemoveQuizzer.QuizStateError quizStateError -> $"Wrong Quiz State: {quizStateError}"
            | RemoveQuizzer.QuizzerNotParticipating quizzer -> $"Quizzer {quizzer} is already not participating"
            | RemoveQuizzer.DbError dbError -> dbError |> mapDbErrorToString

        result |> refreshQuizOrError mapRemoveError
    | SelectQuizzer (Started name) ->
        let command: SelectQuizzer.Command =
            { Quiz = model.Code
              User = model.CurrentUser
              Data = { Quizzer = name } }

        selectQuizzer
        |> Option.map (fun workflow ->
            command
            |> workflow
            |> AsyncResult.map CurrentQuizzerChanged
            |> AsyncResult.map List.singleton
            |> publishWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd SelectQuizzer)
        |> matchOptionalCommand

    | SelectQuizzer (Finished result) ->
        let mapSelectError error =
            match error with
            | (SelectQuizzer.Error.QuizState quizStateError) -> "Quiz is not running"
            | (SelectQuizzer.Error.QuizzerAlreadyCurrent) -> ""
            | (SelectQuizzer.Error.DbError dbError) -> dbError |> mapDbErrorToString
            | (SelectQuizzer.Error.QuizzerNotParticipating quizzer) -> $"Quizzer {quizzer} is not participating"

        result
        |> function
            | Ok result -> result |> refreshModel, Cmd.none, None
            | Result.Error (WorkflowError.Workflow (SelectQuizzer.Error.QuizzerAlreadyCurrent)) -> model, Cmd.none, None
            | Result.Error error ->
                model,
                Cmd.none,
                error
                |> mapWorkflowErrors mapSelectError
                |> ExternalMessage.Error
                |> Some
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
            |> publishWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd AnswerCorrectly)
        |> matchOptionalCommand
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
        let mapEvent event =
            match event with
            | AnswerIncorrectly.Event.CurrentQuizzerChanged e -> RunQuizEvent.CurrentQuizzerChanged e
            | AnswerIncorrectly.Event.IndividualScoreChanged e -> RunQuizEvent.IndividualScoreChanged e
            | AnswerIncorrectly.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        answerIncorrectly
        |> Option.map (fun workflow ->
            workflow
                { Quiz = model.Code
                  User = model.CurrentUser
                  Data = () }
            |> AsyncResult.map (List.map mapEvent)
            |> publishWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd AnswerIncorrectly)
        |> matchOptionalCommand
    | AnswerIncorrectly (Finished result) ->
        let mapIncorrectError error =
            match error with
            | (AnswerIncorrectly.QuizState _) -> "Quiz iz not running"
            | (AnswerIncorrectly.NoCurrentQuizzer _) -> "No current Quizzer"
            | (AnswerIncorrectly.QuizzerAlreadyAnsweredIncorrectly (QuizAnswer.QuizzerAlreadyAnsweredIncorrectly (quizzer,
                                                                                                                  questionNumber))) ->
                $"Quizzer {quizzer} already answered question {questionNumber |> PositiveNumber.value} incorrectly"
            | (AnswerIncorrectly.Error.DbError dbError) -> dbError |> mapDbErrorToString

        result |> refreshQuizOrError mapIncorrectError
    | FailAppeal (Started _) ->
        let mapEvent event =
            match event with
            | FailAppeal.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        failAppeal
        |> Option.map (fun workflow ->
            workflow
                { Quiz = model.Code
                  User = model.CurrentUser
                  Data = () }
            |> AsyncResult.map (List.map mapEvent)
            |> publishWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd FailAppeal)
        |> matchOptionalCommand
    | FailAppeal (Finished quiz) ->
        let mapFailError error =
            match error with
            | (FailAppeal.Error.QuizState _) -> "Wrong Quiz state"
            | (FailAppeal.Error.AppealAlreadyFailed _) -> "Appeal already failed"
            | (FailAppeal.Error.NoCurrentQuizzer _) -> "No current quizzer"
            | (FailAppeal.Error.DbError error) -> error |> mapDbErrorToString

        quiz |> refreshQuizOrError mapFailError
    | ClearAppeal (Started _) ->
        let mapEvent event =
            match event with
            | ClearAppeal.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        clearAppeal
        |> Option.map (fun workflow ->
            workflow
                { Quiz = model.Code
                  Data = ()
                  User = model.CurrentUser }
            |> AsyncResult.map (List.map mapEvent)
            |> publishWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd ClearAppeal)
        |> matchOptionalCommand
    | ClearAppeal (Finished quiz) ->
        let mapAppealError error =
            match error with
            | (ClearAppeal.Error.QuizState _) -> "Wrong Quiz state"
            | (ClearAppeal.Error.NoFailedAppeal _) -> "There is no failed appeal to clear"
            | (ClearAppeal.Error.DbError dbError) -> dbError |> mapDbErrorToString

        quiz |> refreshQuizOrError mapAppealError

let update
    connectToQuizEvents
    onQuizEvent
    (publishQuizEvent: PublishQuizEventTask)
    (getQuizAsync: GetQuiz)
    capabilityProvider
    msg
    model
    =
    match model, msg with
    | Loading (code, user), Message.InitializeQuizAndConnections (Finished result) ->
        match result with
        | Ok quiz -> Loaded(refreshModel (quiz, user)), Cmd.none, None
        | Result.Error error ->
            let externalMessage =
                error |> mapDbErrorToString

            NotYetLoaded (code, user), Cmd.none, ExternalMessage.Error externalMessage |> Some
    | NotYetLoaded (code, user), Message.InitializeQuizAndConnections (Started previousQuizCode) ->
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

        Loading (code,user),
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
            updateLoaded publishQuizEvent getQuizAsync capabilityProvider msg loadedModel

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
    | NotYetLoaded (code, _) -> p { $"Quiz {code} has not yet been loaded" }
    | Loading (code, _) -> p { $"Quiz {code} is loading..." }
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
