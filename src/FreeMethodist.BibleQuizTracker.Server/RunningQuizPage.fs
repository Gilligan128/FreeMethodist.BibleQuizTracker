module FreeMethodist.BibleQuizTracker.Server.RunningQuizPage

open System
open System.Linq.Expressions
open Bolero.Router
open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Capabilities
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView
open FreeMethodist.BibleQuizTracker.Server.Routing
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core



type JumpState =
    | Locked
    | Unlocked

type AddQuizzerModel =
    | Active of string * TeamPosition
    | Inert

type LoadedCompetitionStyle =
    | Team of TeamModel * TeamModel
    | Individuals of QuizzerModel list

type RunQuizCapabilities =
    { AnswerCorrectly: (unit -> AsyncResult<AnswerCorrectly.Event list, AnswerCorrectly.Error>) option
      AnswerIncorrectly: (unit -> AsyncResult<AnswerIncorrectly.Event list, AnswerIncorrectly.Error>) option
      AddQuizzer: (AddQuizzer.Data -> AsyncResult<QuizzerParticipating, AddQuizzer.Error>) option
      RemoveQuizzer: (RemoveQuizzer.Data -> AsyncResult<RemoveQuizzer.Event list, RemoveQuizzer.Error>) option
      FailAppeal: (unit -> AsyncResult<FailAppeal.Event list, FailAppeal.Error>) option
      ClearAppeal: (unit -> AsyncResult<ClearAppeal.Event list, ClearAppeal.Error>) option
      ChangeCurrentQuestion: (ChangeCurrentQuestion.QuestionData -> AsyncResult<CurrentQuestionChanged, ChangeCurrentQuestion.Error>) option
      SelectQuizzer: (SelectQuizzer.Input -> AsyncResult<CurrentQuizzerChanged, SelectQuizzer.Error>) option
      CompleteQuiz: (unit -> AsyncResult<CompleteQuiz.Event list, CompleteQuiz.Error>) option
      ReopenQuiz: (unit -> AsyncResult<ReopenQuiz.Event list, ReopenQuiz.Error>) option }

type LoadedModel =
    { JoiningQuizzer: string
      CompetitionStyle: LoadedCompetitionStyle
      JumpOrder: string list
      CurrentQuestion: int
      JumpState: JumpState
      AddQuizzer: AddQuizzerModel
      CurrentQuizzer: Quizzer option
      NumberOfQuestions: PositiveNumber
      QuestionScores: QuestionQuizzerEvents
      Capabilities: RunQuizCapabilities }

type Model =
    { Code: QuizCode
      User: User
      Info: Deferred<LoadedModel> }

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
    | InitializeQuizAndConnections of AsyncOperationStatus<QuizCode option, Result<Quiz option, DbError>>
    | OnQuizEvent of AsyncOperationStatus<unit, Result<Quiz, DbError>>
    | ChangeCurrentQuestion of AsyncOperationStatus<int, WorkflowResult<ChangeQuestionError>>
    | AddQuizzer of AddQuizzerMessage
    | RemoveQuizzer of
        AsyncOperationStatus<(unit -> AsyncResult<RemoveQuizzer.Event list, RemoveQuizzer.Error>), WorkflowResult<RemoveQuizzer.Error>>
    | SelectQuizzer of AsyncOperationStatus<Quizzer, WorkflowResult<SelectQuizzer.Error>>
    | AnswerCorrectly of AsyncOperationStatus<unit, WorkflowResult<AnswerCorrectly.Error>>
    | AnswerIncorrectly of AsyncOperationStatus<unit, WorkflowResult<AnswerIncorrectly.Error>>
    | FailAppeal of AsyncOperationStatus<unit, WorkflowResult<FailAppeal.Error>>
    | ClearAppeal of AsyncOperationStatus<unit, WorkflowResult<ClearAppeal.Error>>
    | CompleteQuiz of AsyncOperationStatus<unit, WorkflowResult<CompleteQuiz.Error>>
    | ReopenQuiz of AsyncOperationStatus<unit, WorkflowResult<ReopenQuiz.Error>>


type ExternalMessage =
    | ErrorMessage of string
    | NoMessage

let public emptyModel =
    { JoiningQuizzer = ""
      CompetitionStyle = Individuals []
      JumpOrder = []
      CurrentQuestion = 1
      JumpState = Unlocked
      AddQuizzer = Inert
      CurrentQuizzer = None
      NumberOfQuestions = PositiveNumber.one
      QuestionScores = []
      Capabilities =
        { AnswerCorrectly = None
          AnswerIncorrectly = None
          AddQuizzer = None
          RemoveQuizzer = None
          FailAppeal = None
          ClearAppeal = None
          ChangeCurrentQuestion = None
          SelectQuizzer = None
          CompleteQuiz = None
          ReopenQuiz = None } }

let mapLoaded mapper model =
    match model with
    | NotYetStarted _ -> model
    | InProgress _ -> model
    | Resolved loaded -> Resolved(mapper loaded)

let getAppealStateNew appeals quizzer =
    if appeals |> List.contains quizzer then
        AppealFailure
    else
        NoFailure

let private getAnswerState quizAnswer (quizzerState: QuizzerState) =
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

let private refreshQuizzer (currentQuestion: QuestionState) (quizzer: QuizzerState) : QuizzerModel =
    { Name = quizzer.Name
      Score = QuizScore.value quizzer.Score
      ConnectionStatus = Unknown
      AnswerState =
        quizzer
        |> getAnswerState currentQuestion.AnswerState
      AppealState =
        quizzer.Name
        |> getAppealStateNew currentQuestion.FailedAppeals }

let private refreshTeam (currentQuestion: QuestionState) (team: QuizTeamState) : TeamModel =
    { Name = team.Name
      Score = team.Score |> QuizScore.value
      Quizzers =
        team.Quizzers
        |> List.map (refreshQuizzer currentQuestion) }

let refreshCompetitionStyle refreshTeam refreshQuizzer competitionStyle =
    match competitionStyle with
    | RunningCompetitionStyle.Team (teamone, teamTwo) ->
        LoadedCompetitionStyle.Team(refreshTeam teamone, refreshTeam teamTwo)
    | RunningCompetitionStyle.Individuals quizzers ->
        quizzers
        |> List.map refreshQuizzer
        |> LoadedCompetitionStyle.Individuals

let private refreshModel (quiz: RunningQuiz) =
    let currentQuestion =
        quiz.Questions.TryFind(quiz.CurrentQuestion)
        |> Option.defaultValue QuestionState.initial

    let stateMatchedModel =
        { emptyModel with
            CompetitionStyle =
                refreshCompetitionStyle
                    (refreshTeam currentQuestion)
                    (refreshQuizzer currentQuestion)
                    quiz.CompetitionStyle
            CurrentQuestion = PositiveNumber.value quiz.CurrentQuestion
            CurrentQuizzer = quiz.CurrentQuizzer
            JoiningQuizzer = ""
            JumpOrder = [ "Jim"; "Juni"; "John" ]
            JumpState = Unlocked
            NumberOfQuestions = quiz.Questions |> Map.keys |> Seq.max
            QuestionScores = quiz.Questions |> Score.createScoreModel
            }

    stateMatchedModel

let init user quizCode previousQuizCode =
    { Code = quizCode
      User = user
      Info = NotYetStarted },
    Cmd.ofMsg (InitializeQuizAndConnections(Started previousQuizCode))

let private hubStub =
    Unchecked.defaultof<QuizHub.Hub>

let private getAvailableCapabilities (capabilityProvider: RunQuizCapabilityProvider) user currentQuizzerOpt =
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

    let completeQuiz =
        capabilityProvider.CompleteQuiz user

    let reopenQuiz =
        capabilityProvider.ReopenQuiz user

    addQuizzer,
    removeQuizzer,
    answerCorrectly,
    answerIncorrectly,
    failAppeal,
    clearAppeal,
    selectQuizzer,
    changeCurrentQuestion,
    completeQuiz,
    reopenQuiz

let subOfFunc arg (func: 'a -> unit) : Sub<Message> = fun _ -> func arg

let notFoundMessage quizCode =
    $"Quiz {quizCode} not found"
    |> ExternalMessage.ErrorMessage

let getUserFromModel model = model.User

let getCodeFromModel model = model.Code

let mapWorkflowErrors mapWorkflowSpecificError error =
    match error with
    | WorkflowError.DbError error -> error |> mapDbErrorToString
    | WorkflowError.Workflow workflowError -> workflowError |> mapWorkflowSpecificError

let private mapToAsyncOperationCmd asyncOperation resultAsync =
    resultAsync
    |> Async.map (fun result -> asyncOperation (Finished result))
    |> Cmd.OfAsync.result

let private reloadQuizAsync getQuizAsync asyncTask =
    asyncResult {
        do! asyncTask

        let! quiz =
            getQuizAsync ()
            |> AsyncResult.mapError WorkflowError.DbError

        return quiz
    }

let private publishRunQuizEvent publishQuizEvent quiz (event: RunQuizEvent) =
    publishQuizEvent (nameof hubStub.SendRunQuizEventOccurred) quiz event

let private publishEvents publishRunQuizEvent events =
    events
    |> List.map (publishRunQuizEvent)
    |> Async.Parallel
    |> Async.Ignore

let private publishWorkflowEventsAsync publishEvents events =
    asyncResult {
        let! runQuizEvents = events

        do!
            runQuizEvents
            |> publishEvents
            |> AsyncResult.ofAsync
    }
    |> AsyncResult.mapError WorkflowError.Workflow

let startWorkflow getQuiz publishEvents message transformToRunQuizEvent workflow =
    workflow
    |> AsyncResult.map (List.map transformToRunQuizEvent)
    |> publishWorkflowEventsAsync publishEvents
    |> reloadQuizAsync getQuiz
    |> mapToAsyncOperationCmd message


let update
    connectAndHandle
    (publishQuizEvent: PublishQuizEventTask)
    (getQuizAsync: GetQuiz)
    (tryGetQuiz: TryGetQuiz)
    navigate
    capabilityProvider
    msg
    (model: Model)
    : Model * Cmd<Message> * ExternalMessage =
    let quizCode = getCodeFromModel model
    let user = getUserFromModel model

    let refreshModel quiz = refreshModel quiz |> Resolved

    let updateResultWithExternalError error =
        model, Cmd.none, ExternalMessage.ErrorMessage error

    let publishRunQuizEvent quizCode (event: RunQuizEvent) =
        publishQuizEvent (nameof hubStub.SendRunQuizEventOccurred) quizCode event

    let publishEvents =
        publishEvents (publishRunQuizEvent model.Code)

    let getQuiz =
        fun () -> getQuizAsync model.Code

    let startWorkflow message mapToEvent workflow =
        startWorkflow getQuiz publishEvents message mapToEvent workflow

    let finishWorkflow mapWorkflowSpecificErrors result =
        match result with
        | Ok (Running quiz) -> { model with Info = refreshModel quiz }, Cmd.none, NoMessage
        | Ok (Quiz.Completed _)
        | Ok (Official _) ->
            model,
            navigate |> subOfFunc Page.Home |> Cmd.ofSub,
            "Quiz is not running"
            |> ExternalMessage.ErrorMessage
        | Result.Error error ->
            error
            |> mapWorkflowErrors mapWorkflowSpecificErrors
            |> fun errorString -> errorString |> updateResultWithExternalError

    let currentQuizzerOpt =
        (model.Info
         |> function
             | NotYetStarted _ -> None
             | InProgress _ -> None
             | Resolved loaded -> loaded.CurrentQuizzer)

    let matchOptionalCommand cmdOpt =
        let cmd =
            cmdOpt |> Option.defaultValue Cmd.none

        model, cmd, NoMessage

    let (addQuizzer,
         removeQuizzer,
         answerCorrectly,
         answerIncorrectly,
         failAppeal,
         clearAppeal,
         selectQuizzer,
         changeCurrentQuestion,
         completeQuiz,
         reopenQuiz) =
        getAvailableCapabilities capabilityProvider user currentQuizzerOpt

    match msg with
    | Message.InitializeQuizAndConnections (Finished result) ->
        match result with
        | Ok (Some (Running quiz)) ->
            let model =
                { model with Info = quiz |> refreshModel }

            let handleEventSub dispatch _ =
                dispatch (Message.OnQuizEvent(Started()))
                |> Async.retn

            let connectCmd =
                connectAndHandle handleEventSub (quizCode, None)
                |> Cmd.ofSub

            model, connectCmd, NoMessage
        | Ok None -> model, navigate |> subOfFunc Page.Home |> Cmd.ofSub, notFoundMessage quizCode
        | Ok (Some (Quiz.Completed _))
        | Ok (Some (Quiz.Official _)) ->
            model,
            navigate |> subOfFunc Page.Home |> Cmd.ofSub,
            "Quiz is not running"
            |> ExternalMessage.ErrorMessage
        | Result.Error error ->
            let externalMessage =
                error |> mapDbErrorToString

            { model with Info = NotYetStarted }, Cmd.none, ExternalMessage.ErrorMessage externalMessage
    | Message.InitializeQuizAndConnections (Started _) ->
        let loadCmd =
            tryGetQuiz quizCode
            |> Async.timeoutNone 3000
            |> Async.map (fun task ->
                task
                |> Option.defaultValue (Result.Error(DbError.RemoteError "Loading the quiz timed out")))
            |> Async.map (fun result ->
                result
                |> (Message.InitializeQuizAndConnections << Finished))
            |> Cmd.OfAsync.result

        { model with Info = InProgress }, loadCmd, NoMessage
    | OnQuizEvent (Started _) ->
        let getQuizToRefreshCmd =
            getQuizAsync quizCode
            |> Async.map (Finished >> OnQuizEvent)
            |> Cmd.OfAsync.result

        model, getQuizToRefreshCmd, NoMessage
    | OnQuizEvent (Finished (Ok quiz)) ->
        match quiz with
        | Quiz.Running quiz -> { model with Info = refreshModel quiz }, Cmd.none, NoMessage
        | Quiz.Completed _
        | Official _ ->
            model,
            Cmd.none,
            "Quiz is not running"
            |> ExternalMessage.ErrorMessage
    | OnQuizEvent (Finished (Error error)) ->
        let externalMessage =
            error
            |> mapDbErrorToString
            |> ExternalMessage.ErrorMessage

        model, Cmd.none, externalMessage
    | ChangeCurrentQuestion (Started questionNumber) ->
        let mapToQuizEvent event =
            event |> RunQuizEvent.CurrentQuestionChanged

        let startCmd =
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
                            { Quiz = quizCode
                              Data = { Question = newQuestion } }
                        |> AsyncResult.mapError ChangeQuestionError.QuizError
                }
                |> AsyncResult.map List.singleton
                |> startWorkflow ChangeCurrentQuestion mapToQuizEvent)
            |> Option.defaultValue Cmd.none

        model, startCmd, NoMessage
    | ChangeCurrentQuestion (Finished result) ->
        let mapChangeQuestionError error =
            match error with
            | ChangeQuestionError.FormError er -> er
            | ChangeQuestionError.QuizError er -> $"Wrong Quiz State: {er}" in

        result |> finishWorkflow mapChangeQuestionError
    | Message.AddQuizzer Cancel ->
        { model with
            Info =
                model.Info
                |> mapLoaded (fun loaded -> { loaded with AddQuizzer = Inert }) },
        Cmd.none,
        NoMessage
    | Message.AddQuizzer Start ->
        { model with
            Info =
                model.Info
                |> mapLoaded (fun loaded ->
                    { loaded with
                        AddQuizzer =
                            match loaded.AddQuizzer with
                            | Inert -> Active("", TeamOne)
                            | Active (name, team) -> Active(name, team) }) },
        Cmd.none,
        NoMessage
    | AddQuizzer (SetName name) ->
        { model with
            Info =
                model.Info
                |> mapLoaded (fun loaded ->
                    { loaded with
                        AddQuizzer =
                            match loaded.AddQuizzer with
                            | Inert -> Inert
                            | Active (_, team) -> Active(name, team) }) },
        Cmd.none,
        NoMessage
    | AddQuizzer (SetTeam teamPosition) ->
        { model with
            Info =
                model.Info
                |> mapLoaded (fun loaded ->
                    { loaded with
                        AddQuizzer =
                            match loaded.AddQuizzer with
                            | Inert -> Inert
                            | Active (name, _) -> Active(name, teamPosition) }) },
        Cmd.none,
        NoMessage
    | AddQuizzer (Submit (Started _)) ->
        let addQuizzerState model =
            match model with
            | NotYetStarted _ -> Inert
            | InProgress _ -> Inert
            | Resolved loaded -> loaded.AddQuizzer

        match addQuizzerState model.Info with
        | AddQuizzerModel.Active (name, team) ->
            let mapQuizEvent event = event |> QuizzerParticipating

            let inputCommand: AddQuizzer.Command =
                { Quiz = quizCode
                  Data = { Name = name; Team = Some team } }

            let startedCmd =
                addQuizzer
                |> Option.map (fun workflow ->
                    workflow inputCommand
                    |> AsyncResult.map List.singleton
                    |> startWorkflow (AddQuizzerMessage.Submit >> Message.AddQuizzer) mapQuizEvent)
                |> Option.defaultValue Cmd.none

            model, startedCmd, NoMessage
        | AddQuizzerModel.Inert ->
            model, Cmd.none, ExternalMessage.ErrorMessage "How are you even submitting from an inert AddQuizzer state?"
    | AddQuizzer (Submit (Finished result)) ->
        let mapAddQuizzerError error =
            match error with
            | AddQuizzer.Error.QuizState quizStateError -> $"Wrong Quiz State: {quizStateError}"
            | AddQuizzer.Error.QuizzerAlreadyAdded quizzer -> $"Quizzer {quizzer} already added"
            | AddQuizzer.DbError dbError -> dbError |> mapDbErrorToString

        let model, cmd, externalMsg =
            result |> finishWorkflow mapAddQuizzerError

        { model with
            Info =
                model.Info
                |> mapLoaded (fun loaded -> { loaded with AddQuizzer = Inert }) },
        cmd,
        externalMsg
    | RemoveQuizzer (Started cap) ->

        let transformToRunQuizEvent event =
            match event with
            | RemoveQuizzer.Event.CurrentQuizzerChanged e -> RunQuizEvent.CurrentQuizzerChanged e
            | RemoveQuizzer.Event.QuizzerNoLongerParticipating e -> RunQuizEvent.QuizzerNoLongerParticipating e

        removeQuizzer
        |> Option.map (fun workflow ->
            cap ()
            |> startWorkflow RemoveQuizzer transformToRunQuizEvent)
        |> Option.defaultValue Cmd.none
        |> fun cmd -> model, cmd, NoMessage
    | RemoveQuizzer (Finished result) ->
        let mapRemoveError error =
            match error with
            | RemoveQuizzer.QuizStateError quizStateError -> $"Wrong Quiz State: {quizStateError}"
            | RemoveQuizzer.QuizzerNotParticipating quizzer -> $"Quizzer {quizzer} is already not participating"
            | RemoveQuizzer.DbError dbError -> dbError |> mapDbErrorToString

        result |> finishWorkflow mapRemoveError
    | SelectQuizzer (Started name) ->
        let command: SelectQuizzer.Command =
            { Quiz = quizCode
              Data = { Quizzer = name } }

        let transformEvent event = CurrentQuizzerChanged event

        selectQuizzer
        |> Option.map (fun workflow ->
            command
            |> workflow
            |> AsyncResult.map List.singleton
            |> startWorkflow SelectQuizzer transformEvent)
        |> Option.defaultValue Cmd.none
        |> fun cmd -> model, cmd, NoMessage
    | SelectQuizzer (Finished result) ->
        let mapSelectError error =
            match error with
            | SelectQuizzer.Error.QuizState _ -> "Quiz is not running"
            | SelectQuizzer.Error.QuizzerAlreadyCurrent -> ""
            | SelectQuizzer.Error.DbError dbError -> dbError |> mapDbErrorToString
            | SelectQuizzer.Error.QuizzerNotParticipating quizzer -> $"Quizzer {quizzer} is not participating"

        result
        |> function
            | Ok (Running result) -> { model with Info = result |> refreshModel }, Cmd.none, NoMessage
            | Ok (Quiz.Completed _)
            | Ok (Official _) ->
                model,
                (fun _ -> navigate Page.Home) |> Cmd.ofSub,
                "Quiz is not running"
                |> ExternalMessage.ErrorMessage
            | Result.Error (WorkflowError.Workflow SelectQuizzer.Error.QuizzerAlreadyCurrent) ->
                model, Cmd.none, NoMessage
            | Result.Error error ->
                model,
                Cmd.none,
                error
                |> mapWorkflowErrors mapSelectError
                |> ExternalMessage.ErrorMessage
    | AnswerCorrectly (Started _) ->
        let mapEvent event =
            match event with
            | AnswerCorrectly.CurrentQuestionChanged e -> CurrentQuestionChanged e
            | AnswerCorrectly.IndividualScoreChanged e -> IndividualScoreChanged e
            | AnswerCorrectly.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        let workflowOpt =
            answerCorrectly
            |> Option.map (fun workflow -> workflow { Data = (); Quiz = quizCode })

        workflowOpt
        |> Option.map (fun result -> result |> startWorkflow AnswerCorrectly mapEvent)
        |> Option.defaultValue Cmd.none
        |> fun cmd -> model, cmd, NoMessage
    | AnswerCorrectly (Finished result) ->
        let mapWorkflowSpecificErrors workflowError =
            match workflowError with
            | AnswerCorrectly.QuizzerNotFound er -> $"Quizzer {er} was not found in this quiz"
            | AnswerCorrectly.Error.QuizStateError _ -> "Quiz is not running"
            | AnswerCorrectly.Error.NoCurrentQuizzer -> "No one has jumped yet"
            | (AnswerCorrectly.Error.QuizzerAlreadyAnsweredCorrectly (QuizAnswer.QuizzerAlreadyAnsweredCorrectly (quizzer,
                                                                                                                  question))) ->
                $"Quizzer {quizzer} already correctly answered question {question |> PositiveNumber.value}"
            | AnswerCorrectly.Error.DbError dbError -> dbError |> mapDbErrorToString

        result |> finishWorkflow mapWorkflowSpecificErrors

    | AnswerIncorrectly (Started _) ->
        let mapEvent event =
            match event with
            | AnswerIncorrectly.Event.CurrentQuizzerChanged e -> RunQuizEvent.CurrentQuizzerChanged e
            | AnswerIncorrectly.Event.IndividualScoreChanged e -> RunQuizEvent.IndividualScoreChanged e
            | AnswerIncorrectly.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        answerIncorrectly
        |> Option.map (fun workflow ->
            workflow { Quiz = quizCode; Data = () }
            |> startWorkflow AnswerIncorrectly mapEvent)
        |> matchOptionalCommand
    | AnswerIncorrectly (Finished result) ->
        let mapIncorrectError error =
            match error with
            | AnswerIncorrectly.QuizState _ -> "Quiz iz not running"
            | AnswerIncorrectly.NoCurrentQuizzer _ -> "No current Quizzer"
            | (AnswerIncorrectly.QuizzerAlreadyAnsweredIncorrectly (QuizAnswer.QuizzerAlreadyAnsweredIncorrectly (quizzer,
                                                                                                                  questionNumber))) ->
                $"Quizzer {quizzer} already answered question {questionNumber |> PositiveNumber.value} incorrectly"
            | AnswerIncorrectly.Error.DbError dbError -> dbError |> mapDbErrorToString

        result |> finishWorkflow mapIncorrectError
    | FailAppeal (Started _) ->
        let mapEvent event =
            match event with
            | FailAppeal.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e
            | FailAppeal.Event.IndividualScoreChanged e -> RunQuizEvent.IndividualScoreChanged e

        failAppeal
        |> Option.map (fun workflow ->
            workflow { Quiz = quizCode; Data = () }
            |> startWorkflow FailAppeal mapEvent)
        |> matchOptionalCommand
    | FailAppeal (Finished quiz) ->
        let mapFailError error =
            match error with
            | FailAppeal.Error.QuizState _ -> "Wrong Quiz state"
            | FailAppeal.Error.AppealAlreadyFailed _ -> "Appeal already failed"
            | FailAppeal.Error.NoCurrentQuizzer _ -> "No current quizzer"
            | FailAppeal.Error.DbError error -> error |> mapDbErrorToString

        quiz |> finishWorkflow mapFailError
    | ClearAppeal (Started _) ->
        let mapEvent event =
            match event with
            | ClearAppeal.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        clearAppeal
        |> Option.map (fun workflow ->
            workflow { Quiz = quizCode; Data = () }
            |> startWorkflow ClearAppeal mapEvent)
        |> matchOptionalCommand
    | ClearAppeal (Finished quiz) ->
        let mapAppealError error =
            match error with
            | ClearAppeal.Error.QuizState _ -> "Wrong Quiz state"
            | ClearAppeal.Error.NoFailedAppeal _ -> "There is no failed appeal to clear"
            | ClearAppeal.Error.DbError dbError -> dbError |> mapDbErrorToString

        quiz |> finishWorkflow mapAppealError
    | CompleteQuiz (Started _) ->
        let mapQuizEvent event =
            match event with
            | CompleteQuiz.Event.QuizStateChanged e -> RunQuizEvent.QuizStateChanged e

        completeQuiz
        |> Option.map (fun workflow ->
            workflow quizCode
            |> startWorkflow CompleteQuiz mapQuizEvent)
        |> matchOptionalCommand
    | CompleteQuiz (Finished (Ok quiz)) ->
        let newCmd =
            (quizCode, Router.noModel)
            |> Page.QuizDetails
            |> fun page -> (fun _ -> navigate page) |> Cmd.ofSub

        model, newCmd, NoMessage
    | CompleteQuiz (Finished (Error error)) ->
        let mapErrors error =
            match error with
            | CompleteQuiz.Error.DbError dbError -> mapDbErrorToString dbError
            | CompleteQuiz.QuizState quizStateError -> mapQuizStateErrorToString quizStateError

        let errorMessage =
            error
            |> mapWorkflowErrors mapErrors
            |> ExternalMessage.ErrorMessage

        model, Cmd.none, errorMessage
    | ReopenQuiz (Started _) ->
        let mapQuizEvent event =
            match event with
            | ReopenQuiz.Event.QuizStateChanged e -> RunQuizEvent.QuizStateChanged e

        reopenQuiz
        |> Option.map (fun workflow ->
            workflow quizCode
            |> startWorkflow ReopenQuiz mapQuizEvent)
        |> matchOptionalCommand
    | ReopenQuiz (Finished result) ->
        let mapErrors error =
            match error with
            | ReopenQuiz.Error.DbError dbError -> mapDbErrorToString dbError
            | ReopenQuiz.QuizState quizStateError -> mapQuizStateErrorToString quizStateError

        result |> finishWorkflow mapErrors

type private quizPage = Template<"wwwroot/Quiz.html">


let quizzerView removeQuizzerCap dispatch (currentQuizzer: Quizzer option) (quizzer: QuizzerModel, jumpPosition: int) =
    let removeCap =
        removeQuizzerCap |> fun cap -> cap quizzer.Name

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
        .RemoveButton(
            button {
                attr.``class`` "button is-info is-light"

                removeCap |> Html.disabledIfNone

                on.click (fun _ ->
                    removeCap
                    |> Option.iter (fun cap -> cap |> Started |> RemoveQuizzer |> dispatch))

                span {
                    attr.``class`` "icon"
                    i { attr.``class`` "fas fa-times-circle" }
                }
            }
        )
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

let private getJumpPosition jumpOrder (quizzer: QuizzerModel) =
    jumpOrder
    |> Seq.tryFindIndex (fun q -> q = quizzer.Name)
    |> Option.map ((+) 1)
    |> function
        | Some v -> v
        | None -> 0

let private teamView
    removeQuizzerCap
    position
    (quizzerView: QuizzerModel * int -> Node)
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
                        quizzer |> getJumpPosition jumpOrder

                    quizzerView (quizzer, jumpPosition)
            }
        )
        .Elt()

let individualSideView removeQuizzerCap quizzerView (jumpOrder: string list) quizzerModels =
    forEach quizzerModels
    <| fun quizzer ->
        let jumpPosition =
            quizzer |> getJumpPosition jumpOrder

        quizzerView (quizzer, jumpPosition)



let private mapItemizedTeam (team: TeamModel) : ItemizedTeam =
    { Name = team.Name
      Quizzers = team.Quizzers |> List.map (fun q -> q.Name) }

let sideViewSplit (individualsView: QuizzerModel list -> Node) index quizzerModels =
    quizzerModels
    |> List.splitInto 2
    |> List.tryItem index
    |> Option.defaultValue []
    |> individualsView

let render linkToQuiz capabilityProvider (model: Model) (dispatch: Dispatch<Message>) =

    let isTeam model teamOneValue teamTwoValue =
        match model.AddQuizzer with
        | Inert -> false
        | Active (_, TeamOne) -> teamOneValue
        | Active (_, TeamTwo) -> teamTwoValue

    match model.Info with
    | Deferred.NotYetStarted -> p { $"Quiz {model.Code} has not yet been loaded" }
    | InProgress -> p { $"Quiz {model.Code} is loading..." }
    | Resolved resolved ->
        let (addQuizzer,
             removeQuizzer,
             answerCorrectly,
             answerIncorrectly,
             failAppeal,
             clearAppeal,
             selectQuizzer,
             changeCurrentQuestion,
             completeQuiz,
             reopenQuiz) =
            getAvailableCapabilities capabilityProvider model.User resolved.CurrentQuizzer

        let removeQuizzerCap quizzer =
            removeQuizzer
            |> Option.map (fun remove ->
                fun () ->
                    remove
                        { Quiz = model.Code
                          Data = { Quizzer = quizzer } })

        let quizzerView =
            quizzerView removeQuizzerCap dispatch resolved.CurrentQuizzer

        let individualSideView =
            individualSideView removeQuizzerCap quizzerView resolved.JumpOrder

        quizPage()
            .QuizCode(model.Code)
            .QuizUrl(linkToQuiz <| model.Code)
            .CurrentUser(
                match model.User with
                | Quizmaster -> "Quizmaster"
                | Spectator -> "Spectator"
                | Quizzer name -> name
                | Scorekeeper -> "Scorekeeper"
            )
            .SideOne(
                match resolved.CompetitionStyle with
                | LoadedCompetitionStyle.Team (teamOne, teamTwo) ->
                    teamView
                        removeQuizzerCap
                        TeamPosition.TeamOne
                        quizzerView
                        (teamOne, resolved.JumpOrder, resolved.CurrentQuizzer)
                        dispatch
                | LoadedCompetitionStyle.Individuals quizzerModels ->
                    quizzerModels
                    |> sideViewSplit individualSideView 0
            )
            .SideTwo(
                match resolved.CompetitionStyle with
                | LoadedCompetitionStyle.Team (teamOne, teamTwo) ->
                    teamView
                        removeQuizzerCap
                        TeamPosition.TeamTwo
                        quizzerView
                        (teamTwo, resolved.JumpOrder, resolved.CurrentQuizzer)
                        dispatch
                | LoadedCompetitionStyle.Individuals quizzerModels ->
                    quizzerModels
                    |> sideViewSplit individualSideView 1
            )
            .CurrentQuestion(string resolved.CurrentQuestion)
            .NextQuestion(fun _ -> dispatch (ChangeCurrentQuestion(Started(resolved.CurrentQuestion + 1))))
            .UndoQuestion(fun _ -> dispatch (ChangeCurrentQuestion(Started(Math.Max(resolved.CurrentQuestion - 1, 1)))))
            .CurrentQuizzer(
                match resolved.CurrentQuizzer with
                | Some q -> $"{q}'s Turn"
                | None -> ""
            )
            .JumpLockToggleAction(
                match resolved.JumpState with
                | Locked -> "Unlock"
                | Unlocked -> "Lock"
            )
            .AddQuizzerTeamView(
                match resolved.CompetitionStyle with
                | Individuals _ -> Html.empty ()
                | Team (teamOne, teamTwo) ->
                    quizPage
                        .AddQuizzerTeam()
                        .TeamOneName(teamOne.Name)
                        .TeamTwoName(teamTwo.Name)
                        .SetAddQuizzerTeamOne(fun _ -> dispatch (AddQuizzer(SetTeam TeamOne)))
                        .SetAddQuizzerTeamTwo(fun _ -> dispatch (AddQuizzer(SetTeam TeamTwo)))
                        .AddQuizzerIsTeamOne(isTeam resolved true false)
                        .AddQuizzerIsTeamTwo(isTeam resolved false true)
                        .Elt()
            )
            .AddQuizzerName(
                (match resolved.AddQuizzer with
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
                if resolved.AddQuizzer = Inert then
                    ""
                else
                    "is-active"
            )
            .AddQuizzerSubmit(fun _ -> dispatch (AddQuizzer(Submit(Started()))))
            .AnswerCorrectly(fun _ -> dispatch (AnswerCorrectly(Started())))
            .AnswerIncorrectly(fun _ -> dispatch (AnswerIncorrectly(Started())))
            .FailAppeal(fun _ -> dispatch (FailAppeal(Started())))
            .ClearAppeal(fun _ -> dispatch (ClearAppeal(Started())))
            .ItemizedScore(
                ItemizedScore.render
                    { CompetitionStyle =
                        match resolved.CompetitionStyle with
                        | LoadedCompetitionStyle.Team (teamOne, teamTwo) ->
                            (mapItemizedTeam teamOne, mapItemizedTeam teamTwo)
                            |> ItemizedCompetitionStyle.Team
                        | LoadedCompetitionStyle.Individuals quizzers ->
                            quizzers
                            |> List.map (fun q -> q.Name)
                            |> ItemizedCompetitionStyle.Individual
                      NumberOfQuestions = resolved.NumberOfQuestions
                      QuestionsWithEvents = resolved.QuestionScores }
                    dispatch
            )
            .CompleteQuiz(fun _ -> dispatch (CompleteQuiz(Started())))
            .ReopenQuiz(fun _ -> dispatch (ReopenQuiz(Started())))
            .Elt()
