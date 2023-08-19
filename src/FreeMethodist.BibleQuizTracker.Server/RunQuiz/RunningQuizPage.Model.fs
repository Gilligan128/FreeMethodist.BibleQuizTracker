module FreeMethodist.BibleQuizTracker.Server.RunningQuizPage_Model

open FreeMethodist.BibleQuizTracker.Server.Capabilities
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow



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
      ReopenQuiz: (unit -> AsyncResult<ReopenQuiz.Event list, ReopenQuiz.Error>) option
      Prejump: PrejumpCap option }

type JumpOrder =
    | Prejump of Quizzer
    | Standard of Quizzer list

type Modal =
    | AddQuizzer of string * TeamPosition
    | ManageRoster of ManageRoster.Model

type LoadedModel =
    { JoiningQuizzer: string
      CompetitionStyle: LoadedCompetitionStyle
      JumpOrder: string list
      CurrentQuestion: int
      JumpState: JumpState
      CurrentQuizzer: Quizzer option
      NumberOfQuestions: PositiveNumber
      QuestionScores: QuestionQuizzerEvents
      Capabilities: RunQuizCapabilities
      ActiveModal: Modal option }

type Model =
    { Code: QuizCode
      User: User
      Info: Deferred<LoadedModel> }

type WorkflowResult<'a> = Result<Quiz, WorkflowError<'a>>

type ShouldBeRunningError = ShouldBeRunningError


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
    | AddQuizzer of AddQuizzerMessage
    | RemoveQuizzer of
        AsyncOperationStatus<unit -> AsyncResult<RemoveQuizzer.Event list, RemoveQuizzer.Error>, WorkflowResult<RemoveQuizzer.Error>>
    | SelectQuizzer of AsyncOperationStatus<Quizzer, WorkflowResult<SelectQuizzer.Error>>
    | AnswerIncorrectly of AsyncOperationStatus<unit, WorkflowResult<AnswerIncorrectly.Error>>
    | FailAppeal of AsyncOperationStatus<unit, WorkflowResult<FailAppeal.Error>>
    | ClearAppeal of AsyncOperationStatus<unit, WorkflowResult<ClearAppeal.Error>>
    | CompleteQuiz of AsyncOperationStatus<unit, WorkflowResult<CompleteQuiz.Error>>
    | ReopenQuiz of AsyncOperationStatus<unit, WorkflowResult<ReopenQuiz.Error>>
    | ExecuteWorkflow of AsyncOperationStatus<unit -> AsyncResult<RunQuizEvent list, string>, WorkflowResult<string>>
    | ManageRoster of ManageRoster.Message
    | StartManagingRoster of ManageRoster.ModelRoster
    | RefreshQuizFromRoster of Result<Quiz, DbError>


type ExternalMessage =
    | ErrorMessage of string
    | NoMessage
