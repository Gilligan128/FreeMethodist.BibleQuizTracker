module FreeMethodist.BibleQuizTracker.Server.RunningQuizPage_Update

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
open FreeMethodist.BibleQuizTracker.Server.CreateQuiz.Workflow
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView
open FreeMethodist.BibleQuizTracker.Server.Routing
open FreeMethodist.BibleQuizTracker.Server.RunningQuizPage_Model
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core



let public emptyModel =
    { JoiningQuizzer = ""
      CompetitionStyle = LoadedCompetitionStyle.Individuals []
      JumpOrder = []
      CurrentQuestion = 1
      JumpState = Unlocked
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
          ReopenQuiz = None
          Prejump = None }
      ActiveModal = None }

let mapLoaded (mapper: 'T -> 'U) (model: Deferred<'T>) : Deferred<'U> =
    match model with
    | NotYetStarted -> NotYetStarted
    | InProgress -> InProgress
    | Resolved loaded -> Resolved(mapper loaded)

let getAppealStateNew appeals quizzer =
    if appeals |> List.contains quizzer then
        AppealFailure
    else
        NoFailure

let private getAnswerState quizAnswer (quizzerState: QuizzerState) =
    let quizzerWasIncorrect = List.contains quizzerState.Name

    match quizAnswer with
    | Incomplete incorrectAnswerers when incorrectAnswerers |> quizzerWasIncorrect -> AnsweredIncorrectly
    | Incomplete _ -> DidNotAnswer
    | Complete(Answered question) when question.Answerer = quizzerState.Name -> AnsweredCorrectly
    | Complete(Answered question) when question.IncorrectAnswerers |> quizzerWasIncorrect -> AnsweredIncorrectly
    | Complete(Answered _) -> DidNotAnswer
    | Complete(Unanswered question) when question |> quizzerWasIncorrect -> AnsweredIncorrectly
    | Complete(Unanswered _) -> DidNotAnswer

let private refreshQuizzer (currentQuestion: QuestionState) (quizzer: QuizzerState) : QuizzerModel =
    { Name = quizzer.Name
      Score = QuizScore.value quizzer.Score
      ConnectionStatus = Unknown
      AnswerState = quizzer |> getAnswerState currentQuestion.AnswerState
      AppealState = quizzer.Name |> getAppealStateNew currentQuestion.FailedAppeals
      PrejumpState =
        if currentQuestion.Prejumps |> List.contains quizzer.Name then
            Prejumped
        else
            NoPrejump }

let private refreshTeam (currentQuestion: QuestionState) (team: QuizTeamState) : TeamModel =
    { Name = team.Name
      Score = team.Score |> QuizScore.value
      Quizzers = team.Quizzers |> List.map (refreshQuizzer currentQuestion) }

let refreshCompetitionStyle refreshTeam refreshQuizzer competitionStyle =
    match competitionStyle with
    | RunningCompetitionStyle.Team(teamone, teamTwo) ->
        LoadedCompetitionStyle.Team(refreshTeam teamone, refreshTeam teamTwo)
    | RunningCompetitionStyle.Individuals quizzers ->
        quizzers |> List.map refreshQuizzer |> LoadedCompetitionStyle.Individuals

let provideCapabilitiesModel (capabilityProvider: RunQuizCapabilityForQuizProvider) user quiz : RunQuizCapabilities =
    { AddQuizzer = capabilityProvider.AddQuizzer quiz user
      AnswerCorrectly = capabilityProvider.AnswerCorrectly quiz user
      AnswerIncorrectly = capabilityProvider.AnswerIncorrectly quiz user
      RemoveQuizzer = capabilityProvider.RemoveQuizzer quiz user
      FailAppeal = capabilityProvider.FailAppeal quiz user
      ClearAppeal = capabilityProvider.ClearAppeal quiz user
      ChangeCurrentQuestion = capabilityProvider.ChangeCurrentQuestion quiz user
      SelectQuizzer = capabilityProvider.SelectQuizzer quiz user
      CompleteQuiz = capabilityProvider.CompleteQuiz quiz user
      ReopenQuiz = capabilityProvider.ReopenQuiz quiz user
      Prejump = capabilityProvider.Prejump quiz user }

let private refreshRoster competitionStyle : ManageRosterForm.ModelRoster =
    match competitionStyle with
    | RunningCompetitionStyle.Team(teamOne, teamTwo) ->
        let rosters: ManageRosterForm.TeamRoster * ManageRosterForm.TeamRoster =
            { Name = teamOne.Name
              Quizzers = teamOne.Quizzers |> List.map (fun q -> q.Name) },
            { Name = teamTwo.Name
              Quizzers = teamTwo.Quizzers |> List.map (fun q -> q.Name) }

        rosters |> ManageRosterForm.ModelRoster.Team
    | RunningCompetitionStyle.Individuals quizzers ->
        quizzers
        |> List.map (fun q -> q.Name)
        |> ManageRosterForm.ModelRoster.Individual


let private refreshModel capabilitiesProvider (quiz: RunningQuiz, modal) =
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
            ActiveModal =
                match modal with
                | Some(Modal.ManageRoster value) ->
                    { value with
                        Roster = refreshRoster quiz.CompetitionStyle }
                    |> Modal.ManageRoster
                    |> Some
                | _ -> modal
            Capabilities = capabilitiesProvider quiz }

    stateMatchedModel

let init user quizCode previousQuizCode =
    { Code = quizCode
      User = user
      Info = NotYetStarted },
    Cmd.ofMsg (InitializeQuizAndConnections(Started previousQuizCode))

let private hubStub = Unchecked.defaultof<QuizHub.Hub>

let subOfFunc arg (func: 'a -> unit) : Sub<Message> = fun _ -> func arg

let notFoundMessage quizCode =
    $"Quiz {quizCode} not found" |> ExternalMessage.ErrorMessage

let getUserFromModel model = model.User

let getCodeFromModel (model: Model) = model.Code

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

        let! quiz = getQuizAsync () |> AsyncResult.mapError WorkflowError.DbError

        return quiz
    }

let private publishRunQuizEvent publishQuizEvent quiz (event: RunQuizEvent) =
    publishQuizEvent (nameof hubStub.SendRunQuizEventOccurred) quiz event

let private publishEvents publishRunQuizEvent events =
    events |> List.map (publishRunQuizEvent) |> Async.Parallel |> Async.Ignore

let private publishWorkflowEventsAsync publishEvents events =
    asyncResult {
        let! runQuizEvents = events
        do! runQuizEvents |> publishEvents |> AsyncResult.ofAsync
    }
    |> AsyncResult.mapError WorkflowError.Workflow

let startWorkflow getQuiz publishEvents message workflow =
    workflow
    |> publishWorkflowEventsAsync publishEvents
    |> reloadQuizAsync getQuiz
    |> mapToAsyncOperationCmd message

let private shouldBeRunning quiz =
    match quiz with
    | Quiz.Running quiz -> Ok quiz
    | _ -> Error ShouldBeRunningError



let update
    connectAndHandle
    (publishQuizEvent: PublishQuizEventTask)
    (getQuizAsync: GetQuiz)
    (tryGetQuiz: TryGetQuiz)
    navigate
    (capabilityForQuizProvider: RunQuizCapabilityForQuizProvider)
    msg
    (model: Model)
    : Model * Cmd<Message> * ExternalMessage =
    let quizCode = getCodeFromModel model
    let user = getUserFromModel model

    let provideCapabilities = provideCapabilitiesModel capabilityForQuizProvider user

    let refreshModel quiz =
        refreshModel provideCapabilities quiz |> Resolved

    let updateResultWithExternalError error =
        model, Cmd.none, ExternalMessage.ErrorMessage error

    let publishRunQuizEvent quizCode (event: RunQuizEvent) =
        publishQuizEvent (nameof hubStub.SendRunQuizEventOccurred) quizCode event

    let publishEvents = publishEvents (publishRunQuizEvent model.Code)

    let getQuiz = fun () -> getQuizAsync model.Code

    let startWorkflow message workflow =
        startWorkflow getQuiz publishEvents message workflow

    let finishWorkflowCore result =
        (match result with
         | Ok quiz -> quiz |> shouldBeRunning |> Result.mapError Choice1Of2
         | Error error -> error |> Choice2Of2 |> Error)
        |> fun result ->
            match result with
            | Ok quiz ->
                let activeModal =
                    model.Info |> Deferred.toOption |> Option.bind (fun model -> model.ActiveModal)

                { model with
                    Info = refreshModel (quiz, activeModal) },
                Cmd.none,
                ExternalMessage.NoMessage
            | Error(Choice1Of2 _) ->
                model,
                navigate |> subOfFunc Page.Home |> Cmd.ofSub,
                "Quiz is not running" |> ExternalMessage.ErrorMessage
            | Error(Choice2Of2 errorString) -> errorString |> updateResultWithExternalError

    let finishWorkflow mapWorkflowSpecificErrors result =
        result
        |> Result.mapError (mapWorkflowErrors mapWorkflowSpecificErrors)
        |> finishWorkflowCore

    let matchOptionalCommand cmdOpt =
        let cmd = cmdOpt |> Option.defaultValue Cmd.none

        model, cmd, NoMessage

    let activeModal =
        model.Info |> Deferred.toOption |> Option.bind (fun model -> model.ActiveModal)

    match msg with
    | Message.InitializeQuizAndConnections(Finished result) ->
        match result with
        | Ok(Some(Quiz.Running quiz)) ->
            let activeModal =
                model.Info |> Deferred.toOption |> Option.bind (fun model -> model.ActiveModal)

            let model =
                { model with
                    Info = (quiz, activeModal) |> refreshModel }

            let handleEventSub dispatch _ =
                dispatch (Message.OnQuizEvent(Started())) |> Async.retn

            let connectCmd = connectAndHandle handleEventSub (quizCode, None) |> Cmd.ofSub

            model, connectCmd, NoMessage
        | Ok None -> model, navigate |> subOfFunc Page.Home |> Cmd.ofSub, notFoundMessage quizCode
        | Ok(Some(Quiz.Completed _))
        | Ok(Some(Quiz.Official _)) ->
            model, navigate |> subOfFunc Page.Home |> Cmd.ofSub, "Quiz is not running" |> ExternalMessage.ErrorMessage
        | Result.Error error ->
            let externalMessage = error |> mapDbErrorToString

            { model with Info = NotYetStarted }, Cmd.none, ExternalMessage.ErrorMessage externalMessage
    | Message.InitializeQuizAndConnections(Started _) ->
        let loadCmd =
            tryGetQuiz quizCode
            |> Async.timeoutNone 3000
            |> Async.map (fun task ->
                task
                |> Option.defaultValue (Result.Error(DbError.RemoteError "Loading the quiz timed out")))
            |> Async.map (fun result -> result |> (Message.InitializeQuizAndConnections << Finished))
            |> Cmd.OfAsync.result

        { model with Info = InProgress }, loadCmd, NoMessage
    | OnQuizEvent(Started _) ->
        let getQuizToRefreshCmd =
            getQuizAsync quizCode
            |> Async.map (Finished >> OnQuizEvent)
            |> Cmd.OfAsync.result

        model, getQuizToRefreshCmd, NoMessage
    | OnQuizEvent(Finished(Ok quiz)) ->
        match quiz with
        | Quiz.Running quiz ->
            { model with
                Info = refreshModel (quiz, activeModal) },
            Cmd.none,
            NoMessage
        | Quiz.Completed _
        | Quiz.Official _ -> model, Cmd.none, "Quiz is not running" |> ExternalMessage.ErrorMessage
    | OnQuizEvent(Finished(Error error)) ->
        let externalMessage = error |> mapDbErrorToString |> ExternalMessage.ErrorMessage

        model, Cmd.none, externalMessage
    | StartManagingRoster message ->
        let modal, cmd =
            let capabilities: ManageRosterForm.ManageRosterCapabilities option =
                model.Info
                |> Deferred.toOption
                |> Option.map (fun model ->
                    { RemoveQuizzer = model.Capabilities.RemoveQuizzer
                      AddQuizzer = model.Capabilities.AddQuizzer })

            ManageRosterForm.init
                (capabilities
                 |> Option.defaultValue
                     { RemoveQuizzer = None
                       AddQuizzer = None })
                message

        let info =
            model.Info
            |> mapLoaded (fun m ->
                { m with
                    ActiveModal = Modal.ManageRoster modal |> Some })

        { model with Info = info }, cmd |> Cmd.map Message.ManageRoster, NoMessage
    | ManageRoster message ->
        match model.Info with
        | NotYetStarted -> model, Cmd.none, NoMessage
        | InProgress -> model, Cmd.none, NoMessage
        | Resolved loaded ->
            match loaded.ActiveModal with
            | Some(Modal.ManageRoster modal) ->
                let modelOption, cmd, external = ManageRosterForm.update message modal

                let newLoaded =
                    { loaded with
                        ActiveModal = modelOption |> Option.map Modal.ManageRoster }

                let wflwCmd, externMsg =
                    match external with
                    | ManageRosterForm.ExternalMessage.WorkflowSuccess events ->
                        let cmd =
                            events
                            |> publishEvents
                            |> AsyncResult.ofAsync
                            |> AsyncResult.bind (fun _ -> getQuiz ())
                            |> Async.map (RefreshQuizFromRoster)
                            |> Cmd.OfAsync.result

                        cmd, NoMessage
                    | ManageRosterForm.ExternalMessage.NoOp -> Cmd.none, ExternalMessage.NoMessage

                { model with Info = Resolved newLoaded },
                Cmd.batch [ cmd |> Cmd.map Message.ManageRoster; wflwCmd ],
                externMsg
            | _ -> model, Cmd.none, NoMessage
    | RefreshQuizFromRoster quizResult ->
        match quizResult with
        | Ok quiz ->
            match quiz with
            | Quiz.Running quiz ->

                let newInfo =
                    (quiz, activeModal)
                    |> refreshModel
                { model with Info = newInfo }, Cmd.none, NoMessage
            | _ -> model, navigate |> subOfFunc Page.Home |> Cmd.ofSub, NoMessage
        | Error error -> model, Cmd.none, mapDbErrorToString error |> ExternalMessage.ErrorMessage
    | SelectQuizzer(Started name) ->
        let mapEvent event = CurrentQuizzerChanged event

        model.Info
        |> Deferred.toOption
        |> Option.bind (fun model -> model.Capabilities.SelectQuizzer)
        |> Option.map (fun workflow -> workflow { Quizzer = name })
        |> Option.map (fun workflow ->
            workflow
            |> AsyncResult.map mapEvent
            |> AsyncResult.map List.singleton
            |> startWorkflow SelectQuizzer)
        |> Option.defaultValue Cmd.none
        |> fun cmd -> model, cmd, NoMessage
    | SelectQuizzer(Finished result) ->
        let mapSelectError error =
            match error with
            | SelectQuizzer.Error.QuizState _ -> "Quiz is not running"
            | SelectQuizzer.Error.QuizzerAlreadyCurrent -> ""
            | SelectQuizzer.Error.DbError dbError -> dbError |> mapDbErrorToString
            | SelectQuizzer.Error.QuizzerNotParticipating quizzer -> $"Quizzer {quizzer} is not participating"

        result
        |> function
            | Ok(Quiz.Running result) ->
                { model with
                    Info = (result, activeModal) |> refreshModel },
                Cmd.none,
                NoMessage
            | Ok(Quiz.Completed _)
            | Ok(Quiz.Official _) ->
                model, (fun _ -> navigate Page.Home) |> Cmd.ofSub, "Quiz is not running" |> ExternalMessage.ErrorMessage
            | Result.Error(WorkflowError.Workflow SelectQuizzer.Error.QuizzerAlreadyCurrent) ->
                model, Cmd.none, NoMessage
            | Result.Error (error: WorkflowError<SelectQuizzer.Error>) ->
                model, Cmd.none, error |> mapWorkflowErrors mapSelectError |> ExternalMessage.ErrorMessage
    | AnswerIncorrectly(Started _) ->
        let mapEvent event =
            match event with
            | AnswerIncorrectly.Event.CurrentQuizzerChanged e -> RunQuizEvent.CurrentQuizzerChanged e
            | AnswerIncorrectly.Event.IndividualScoreChanged e -> RunQuizEvent.IndividualScoreChanged e
            | AnswerIncorrectly.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        model.Info
        |> Deferred.toOption
        |> Option.bind (fun model -> model.Capabilities.AnswerIncorrectly)
        |> Option.map (fun workflowCap -> workflowCap ())
        |> Option.map (fun workflow ->
            workflow
            |> AsyncResult.map (List.map mapEvent)
            |> startWorkflow AnswerIncorrectly)
        |> matchOptionalCommand
    | AnswerIncorrectly(Finished result) ->
        let mapIncorrectError error =
            match error with
            | AnswerIncorrectly.QuizState _ -> "Quiz iz not running"
            | AnswerIncorrectly.NoCurrentQuizzer _ -> "No current Quizzer"
            | (AnswerIncorrectly.QuizzerAlreadyAnsweredIncorrectly(QuizAnswer.QuizzerAlreadyAnsweredIncorrectly(quizzer,
                                                                                                                questionNumber))) ->
                $"Quizzer {quizzer} already answered question {questionNumber |> PositiveNumber.value} incorrectly"
            | AnswerIncorrectly.Error.DbError dbError -> dbError |> mapDbErrorToString

        result |> finishWorkflow mapIncorrectError
    | FailAppeal(Started _) ->
        let mapEvent event =
            match event with
            | FailAppeal.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e
            | FailAppeal.Event.IndividualScoreChanged e -> RunQuizEvent.IndividualScoreChanged e

        model.Info
        |> Deferred.toOption
        |> Option.bind (fun model -> model.Capabilities.FailAppeal)
        |> Option.map (fun workflowCap -> workflowCap ())
        |> Option.map (AsyncResult.map (List.map mapEvent))
        |> Option.map (fun workflow -> workflow |> startWorkflow FailAppeal)
        |> matchOptionalCommand
    | FailAppeal(Finished quiz) ->
        let mapFailError error =
            match error with
            | FailAppeal.Error.QuizState _ -> "Wrong Quiz state"
            | FailAppeal.Error.AppealAlreadyFailed _ -> "Appeal already failed"
            | FailAppeal.Error.NoCurrentQuizzer _ -> "No current quizzer"
            | FailAppeal.Error.DbError error -> error |> mapDbErrorToString

        quiz |> finishWorkflow mapFailError
    | ClearAppeal(Started _) ->
        let mapEvent event =
            match event with
            | ClearAppeal.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        model.Info
        |> Deferred.toOption
        |> Option.bind (fun model -> model.Capabilities.ClearAppeal)
        |> Option.map (fun workflowCap -> workflowCap ())
        |> Option.map (fun workflow -> workflow |> AsyncResult.map (List.map mapEvent) |> startWorkflow ClearAppeal)
        |> matchOptionalCommand
    | ClearAppeal(Finished quiz) ->
        let mapAppealError error =
            match error with
            | ClearAppeal.Error.QuizState _ -> "Wrong Quiz state"
            | ClearAppeal.Error.NoFailedAppeal -> "There is no failed appeal to clear"
            | ClearAppeal.Error.DbError dbError -> dbError |> mapDbErrorToString

        quiz |> finishWorkflow mapAppealError
    | CompleteQuiz(Started _) ->
        let mapQuizEvent event =
            match event with
            | CompleteQuiz.Event.QuizStateChanged e -> RunQuizEvent.QuizStateChanged e

        model.Info
        |> Deferred.toOption
        |> Option.bind (fun model -> model.Capabilities.CompleteQuiz)
        |> Option.map (fun workflowCap -> workflowCap ())
        |> Option.map (fun workflow ->
            workflow
            |> AsyncResult.map (List.map mapQuizEvent)
            |> startWorkflow CompleteQuiz)
        |> matchOptionalCommand
    | CompleteQuiz(Finished(Ok _)) ->
        let newCmd =
            (quizCode, Router.noModel)
            |> Page.QuizDetails
            |> fun page -> (fun _ -> navigate page) |> Cmd.ofSub

        model, newCmd, NoMessage
    | CompleteQuiz(Finished(Error error)) ->
        let mapErrors error =
            match error with
            | CompleteQuiz.Error.DbError dbError -> mapDbErrorToString dbError
            | CompleteQuiz.QuizState quizStateError -> mapQuizStateErrorToString quizStateError

        let errorMessage =
            error |> mapWorkflowErrors mapErrors |> ExternalMessage.ErrorMessage

        model, Cmd.none, errorMessage
    | ReopenQuiz(Started _) ->
        let mapQuizEvent event =
            match event with
            | ReopenQuiz.Event.QuizStateChanged e -> RunQuizEvent.QuizStateChanged e

        model.Info
        |> Deferred.toOption
        |> Option.bind (fun model -> model.Capabilities.ReopenQuiz)
        |> Option.map (fun workflowCap -> workflowCap ())
        |> Option.map (fun workflow -> workflow |> AsyncResult.map (List.map mapQuizEvent) |> startWorkflow ReopenQuiz)
        |> matchOptionalCommand
    | ReopenQuiz(Finished result) ->
        let mapErrors error =
            match error with
            | ReopenQuiz.Error.DbError dbError -> mapDbErrorToString dbError
            | ReopenQuiz.QuizState quizStateError -> mapQuizStateErrorToString quizStateError

        result |> finishWorkflow mapErrors
    | ExecuteWorkflow(Started capability) ->
        let cmd = capability () |> startWorkflow ExecuteWorkflow

        model, cmd, NoMessage
    | ExecuteWorkflow(Finished result) -> result |> finishWorkflow id
