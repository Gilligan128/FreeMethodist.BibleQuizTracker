module FreeMethodist.BibleQuizTracker.Server.RunningQuizPage

open System
open System.Linq.Expressions
open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Capabilities.Capabilities
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
    | NotYetStarted of QuizCode * User
    | Loading of QuizCode * User
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
    | InitializeQuizAndConnections of AsyncOperationStatus<QuizCode option, Result<Quiz option, DbError>>
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
    | CompleteQuiz of AsyncOperationStatus<unit, WorkflowResult<CompleteQuiz.Error>>
    | ReopenQuiz of AsyncOperationStatus<unit, WorkflowResult<ReopenQuiz.Error>>


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


let mapLoaded mapper model =
    match model with
    | NotYetStarted _ -> model
    | Loading _ -> model
    | Loaded loaded -> Loaded(mapper loaded)

let private refreshModel (quiz: RunningTeamQuiz, user: User) =
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

    let refreshQuizzer (currentQuestion: QuestionState) (quizzer: QuizzerState) : QuizzerModel =
        { Name = quizzer.Name
          Score = TeamScore.value quizzer.Score
          ConnectionStatus = Unknown
          AnswerState =
            quizzer
            |> getAnswerState currentQuestion.AnswerState
          AppealState =
            quizzer.Name
            |> getAppealState currentQuestion.FailedAppeal }

    let refreshTeam (currentQuestion: QuestionState) (team: QuizTeamState) : TeamModel =
        { Name = team.Name
          Score = team.Score |> TeamScore.value
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
        let currentQuestion =
            quiz.Questions.TryFind(quiz.CurrentQuestion)
            |> Option.defaultValue QuestionState.initial

        { emptyModel with
            Code = quiz.Code
            TeamOne = quiz.TeamOne |> refreshTeam currentQuestion
            TeamTwo = quiz.TeamTwo |> refreshTeam currentQuestion
            CurrentQuestion = PositiveNumber.value quiz.CurrentQuestion
            CurrentQuizzer = quiz.CurrentQuizzer
            CurrentUser = Quizmaster
            JoiningQuizzer = ""
            JumpOrder = [ "Jim"; "Juni"; "John" ]
            JumpState = Unlocked
            QuestionScores =
                quiz.Questions
                |> sortedList
                |> List.map refreshQuestionScore }

    { stateMatchedModel with CurrentUser = user }

let init user quizCode previousQuizCode =
    NotYetStarted(quizCode, user), Cmd.ofMsg (InitializeQuizAndConnections(Started previousQuizCode))

let private hubStub =
    Unchecked.defaultof<QuizHub.Hub>

let private workflowFormError =
    PublishEventError.FormError >> WorkflowError

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
    |> ExternalMessage.Error
    |> Some

let getUserFromModel model =
    match model with
    | NotYetStarted (code, user) -> user
    | Loading (code, user) -> user
    | Loaded loaded -> loaded.CurrentUser

let getCodeFromModel model =
    match model with
    | NotYetStarted (code, user) -> code
    | Loading (code, user) -> code
    | Loaded loaded -> loaded.Code

let update
    connectAndHandle
    (publishQuizEvent: PublishQuizEventTask)
    (getQuizAsync: GetQuiz)
    (tryGetQuiz: TryGetQuiz)
    navigate
    capabilityProvider
    msg
    (model: Model)
    : Model * Cmd<Message> * ExternalMessage option =
    let quizCode = getCodeFromModel model
    let user = getUserFromModel model

    let refreshModel quiz =
        let user = getUserFromModel model
        refreshModel (quiz, user) |> Loaded

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
        |> List.map (publishRunQuizEvent quizCode)
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
                quizCode
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
        | Ok (Running quiz) -> refreshModel quiz, Cmd.none, None
        | Ok (Quiz.Completed _)
        | Ok (Official _) ->
            model,
            navigate |> subOfFunc Page.Home |> Cmd.ofSub,
            "Quiz is not running"
            |> ExternalMessage.Error
            |> Some
        | Result.Error error ->
            error
            |> mapWorkflowErrors mapWorkflowSpecificErrors
            |> fun errorString -> errorString |> updateResultWithExternalError

    let currentQuizzerOpt =
        (model
         |> function
             | NotYetStarted _ -> None
             | Loading _ -> None
             | Loaded loaded -> loaded.CurrentQuizzer)

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
        | Ok ((Some (Running quiz))) ->
            let model = quiz |> refreshModel

            let handleEventSub dispatch _ =
                getQuizAsync quizCode
                |> AsyncResult.map (fun quiz -> dispatch (Message.OnQuizEvent(Finished quiz)))
                |> Async.Ignore

            let connectCmd =
                connectAndHandle handleEventSub (quizCode, None)
                |> Cmd.ofSub

            model, connectCmd, None
        | Ok (None) -> model, navigate |> subOfFunc Page.Home |> Cmd.ofSub, notFoundMessage quizCode
        | Ok (Some (Quiz.Completed _))
        | Ok (Some (Quiz.Official _)) ->
            model,
            navigate |> subOfFunc Page.Home |> Cmd.ofSub,
            "Quiz is not running"
            |> ExternalMessage.Error
            |> Some
        | Result.Error error ->
            let externalMessage =
                error |> mapDbErrorToString

            NotYetStarted(quizCode, user), Cmd.none, ExternalMessage.Error externalMessage |> Some
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

        Loading(quizCode, user), loadCmd, None
    | OnQuizEvent (Started _) ->
        let getQuizToRefresh =
            getQuizAsync quizCode
            |> Async.map (fun result ->
                match result with
                | Ok quiz -> quiz |> (Finished >> OnQuizEvent)
                | Result.Error error -> error |> mapDbErrorToString |> workflowFormError)

        model, Cmd.OfAsync.result getQuizToRefresh, None
    | OnQuizEvent (Finished quiz) ->
        match quiz with
        | Quiz.Running quiz -> refreshModel quiz, Cmd.none, None
        | Quiz.Completed _
        | Official _ ->
            model,
            Cmd.none,
            "Quiz is not running"
            |> ExternalMessage.Error
            |> Some
    | Message.WorkflowError error ->
        let errorMessage =
            match error with
            | PublishEventError.FormError er -> er
            | RemoteError exn -> exn.Message

        model, Cmd.none, errorMessage |> ExternalMessage.Error |> Some
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
                            { Quiz = quizCode
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
    | Message.AddQuizzer Cancel ->
        model
        |> mapLoaded (fun loaded -> { loaded with AddQuizzer = Inert }),
        Cmd.none,
        None
    | Message.AddQuizzer Start ->
        model
        |> mapLoaded (fun loaded ->
            { loaded with
                AddQuizzer =
                    match loaded.AddQuizzer with
                    | Inert -> Active("", TeamOne)
                    | Active (name, team) -> Active(name, team) }),
        Cmd.none,
        None
    | AddQuizzer (SetName name) ->
        model
        |> mapLoaded (fun loaded ->
            { loaded with
                AddQuizzer =
                    match loaded.AddQuizzer with
                    | Inert -> Inert
                    | Active (_, team) -> Active(name, team) }),
        Cmd.none,
        None
    | AddQuizzer (SetTeam teamPosition) ->

        model
        |> mapLoaded (fun loaded ->
            { loaded with
                AddQuizzer =
                    match loaded.AddQuizzer with
                    | Inert -> Inert
                    | Active (name, _) -> Active(name, teamPosition) }),
        Cmd.none,
        None
    | AddQuizzer (Submit (Started _)) ->
        let addQuizzerState model =
            match model with
            | NotYetStarted _ -> Inert
            | Loading _ -> Inert
            | Loaded loaded -> loaded.AddQuizzer

        match addQuizzerState model with
        | AddQuizzerModel.Active (name, team) ->
            let mapQuizEvent event = event |> QuizzerParticipating

            let inputCommand: AddQuizzer.Command =
                { Quiz = quizCode
                  Data = { Name = name; Team = team } }

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

        let model, cmd, externalMsg =
            result |> refreshQuizOrError mapAddQuizzerError

        model
        |> mapLoaded (fun loaded -> { loaded with AddQuizzer = Inert }),
        cmd,
        externalMsg
    | RemoveQuizzer (Started (name, teamPosition)) ->
        let withinQuizCommand: RemoveQuizzer.Command =
            { Quiz = quizCode
              Data = { Quizzer = name; Team = teamPosition } }

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
            { Quiz = quizCode
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
            | SelectQuizzer.Error.QuizState quizStateError -> "Quiz is not running"
            | SelectQuizzer.Error.QuizzerAlreadyCurrent -> ""
            | SelectQuizzer.Error.DbError dbError -> dbError |> mapDbErrorToString
            | SelectQuizzer.Error.QuizzerNotParticipating quizzer -> $"Quizzer {quizzer} is not participating"

        result
        |> function
            | Ok (Running result) -> result |> refreshModel, Cmd.none, None
            | Ok (Quiz.Completed _)
            | Ok (Official _) ->
                model,
                (fun _ -> navigate Page.Home) |> Cmd.ofSub,
                "Quiz is not running"
                |> ExternalMessage.Error
                |> Some
            | Result.Error (WorkflowError.Workflow SelectQuizzer.Error.QuizzerAlreadyCurrent) -> model, Cmd.none, None
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
            |> Option.map (fun workflow -> workflow { Data = (); Quiz = quizCode })


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
            | AnswerCorrectly.Error.DuplicateQuizzer er -> $"There is more than one quizzer with name {er}"
            | AnswerCorrectly.QuizzerNotFound er -> $"Quizzer {er} was not found in this quiz"
            | AnswerCorrectly.Error.QuizStateError _ -> "Quiz is not running"
            | AnswerCorrectly.Error.NoCurrentQuizzer -> "No one has jumped yet"
            | (AnswerCorrectly.Error.QuizzerAlreadyAnsweredCorrectly (QuizAnswer.QuizzerAlreadyAnsweredCorrectly (quizzer,
                                                                                                                  question))) ->
                $"Quizzer {quizzer} already correctly answered question {question |> PositiveNumber.value}"
            | AnswerCorrectly.Error.DbError dbError -> dbError |> mapDbErrorToString

        result
        |> refreshQuizOrError mapWorkflowSpecificErrors

    | AnswerIncorrectly (Started _) ->
        let mapEvent event =
            match event with
            | AnswerIncorrectly.Event.CurrentQuizzerChanged e -> RunQuizEvent.CurrentQuizzerChanged e
            | AnswerIncorrectly.Event.IndividualScoreChanged e -> RunQuizEvent.IndividualScoreChanged e
            | AnswerIncorrectly.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        answerIncorrectly
        |> Option.map (fun workflow ->
            workflow { Quiz = quizCode; Data = () }
            |> AsyncResult.map (List.map mapEvent)
            |> publishWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd AnswerIncorrectly)
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

        result |> refreshQuizOrError mapIncorrectError
    | FailAppeal (Started _) ->
        let mapEvent event =
            match event with
            | FailAppeal.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        failAppeal
        |> Option.map (fun workflow ->
            workflow { Quiz = quizCode; Data = () }
            |> AsyncResult.map (List.map mapEvent)
            |> publishWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd FailAppeal)
        |> matchOptionalCommand
    | FailAppeal (Finished quiz) ->
        let mapFailError error =
            match error with
            | FailAppeal.Error.QuizState _ -> "Wrong Quiz state"
            | FailAppeal.Error.AppealAlreadyFailed _ -> "Appeal already failed"
            | FailAppeal.Error.NoCurrentQuizzer _ -> "No current quizzer"
            | FailAppeal.Error.DbError error -> error |> mapDbErrorToString

        quiz |> refreshQuizOrError mapFailError
    | ClearAppeal (Started _) ->
        let mapEvent event =
            match event with
            | ClearAppeal.Event.TeamScoreChanged e -> RunQuizEvent.TeamScoreChanged e

        clearAppeal
        |> Option.map (fun workflow ->
            workflow { Quiz = quizCode; Data = () }
            |> AsyncResult.map (List.map mapEvent)
            |> publishWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd ClearAppeal)
        |> matchOptionalCommand
    | ClearAppeal (Finished quiz) ->
        let mapAppealError error =
            match error with
            | ClearAppeal.Error.QuizState _ -> "Wrong Quiz state"
            | ClearAppeal.Error.NoFailedAppeal _ -> "There is no failed appeal to clear"
            | ClearAppeal.Error.DbError dbError -> dbError |> mapDbErrorToString

        quiz |> refreshQuizOrError mapAppealError
    | CompleteQuiz (Started _) ->
        let mapQuizEvent event =
            match event with
            | CompleteQuiz.Event.QuizStateChanged e -> RunQuizEvent.QuizStateChanged e


        completeQuiz
        |> Option.map (fun workflow ->
            workflow quizCode
            |> AsyncResult.map (List.map mapQuizEvent)
            |> publishWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd (CompleteQuiz))
        |> matchOptionalCommand
    | CompleteQuiz (Finished result) ->
        let mapErrors error =
            match error with
            | CompleteQuiz.Error.DbError dbError -> mapDbErrorToString dbError
            | CompleteQuiz.QuizState quizStateError -> mapQuizStateErrorToString quizStateError

        result |> refreshQuizOrError mapErrors
    | ReopenQuiz (Started _) ->
        let mapQuizEvent event =
            match event with
            | ReopenQuiz.Event.QuizStateChanged e -> RunQuizEvent.QuizStateChanged e

        reopenQuiz
        |> Option.map (fun workflow ->
            workflow quizCode
            |> AsyncResult.map (List.map mapQuizEvent)
            |> publishWorkflowEventsAsync
            |> reloadQuizAsync
            |> mapToAsyncOperationCmd (ReopenQuiz))
        |> matchOptionalCommand
    | ReopenQuiz (Finished result) ->
        let mapErrors error =
            match error with
            | ReopenQuiz.Error.DbError dbError -> mapDbErrorToString dbError
            | ReopenQuiz.QuizState quizStateError -> mapQuizStateErrorToString quizStateError

        result |> refreshQuizOrError mapErrors

type private quizPage = Template<"wwwroot/Quiz.html">


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

let questionsBetter (scores: Map<Quizzer, AnswerState * AppealState> list) =
    scores
    |> List.indexed
    |> List.collect (fun (index, map) ->
        map
        |> Map.toList
        |> List.map (fun (quizzer, (answerState, appealState)) ->
            let questionNumber =
                PositiveNumber.numberOrOne (index + 1)

            { Position = (questionNumber, quizzer)
              State =
                { AppealState = appealState
                  AnswerState = answerState } }))

let page linkToQuiz (model: Model) (dispatch: Dispatch<Message>) =
    let isTeam model teamOneValue teamTwoValue =
        match model.AddQuizzer with
        | Inert -> false
        | Active (_, TeamOne) -> teamOneValue
        | Active (_, TeamTwo) -> teamTwoValue

    match model with
    | NotYetStarted (code, _) -> p { $"Quiz {code} has not yet been loaded" }
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
                ItemizedScore.render
                    { TeamOne = model.TeamOne
                      TeamTwo = model.TeamTwo
                      Questions = questionsBetter model.QuestionScores
                    }
                    dispatch
            )
            .CompleteQuiz(fun _ -> dispatch (CompleteQuiz(Started())))
            .ReopenQuiz(fun _ -> dispatch (ReopenQuiz(Started())))
            .Elt()
