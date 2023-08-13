module FreeMethodist.BibleQuizTracker.Server.Common_Page

open System
open Elmish
open FreeMethodist.BibleQuizTracker.Server.Capabilities
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Tournament
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core



type AsyncOperationStatus<'started, 'finished> =
    | Started of 'started
    | Finished of 'finished

type Deferred<'T> =
    | NotYetStarted
    | InProgress
    | Resolved of 'T


type ConnectionStatus =
    | Connected
    | Disconnected of DateTimeOffset
    | Unknown

type PrejumpState =
    | NoPrejump
    | Prejumped

type QuizzerModel =
    { Name: string
      Score: int
      ConnectionStatus: ConnectionStatus
      AnswerState: AnswerState
      AppealState: AppealState
      PrejumpState: PrejumpState }

type TeamModel =
    { Name: string
      Score: int
      Quizzers: QuizzerModel list }


let mapDbErrorToString error =
    match error with
    | Exception exn -> exn.Message
    | DbError.RemoteError message -> message

let mapQuizStateErrorToString _ = "Wrong quiz state"

type WorkflowError<'a> =
    | Workflow of 'a
    | DbError of DbError


//Itemized Score model
type ItemizedTeam =
    { Name: string; Quizzers: Quizzer list }

type ItemizedCompetitionStyle =
    | Individual of Quizzer list
    | Team of ItemizedTeam * ItemizedTeam

type ItemizedScoreModel =
    { CompetitionStyle: ItemizedCompetitionStyle
      NumberOfQuestions: PositiveNumber
      QuestionsWithEvents: QuestionQuizzerEvents }

//Quiz Details

type Link = string

type QuizControlCapabilities =
    { CompleteQuiz: CompleteQuizCap option
      ReopenQuiz: ReopenQuizCap option
      Run: Link option
      Spectate: Link option
      LiveScore: Link option }

type Details =
    { State: string
      Capabilities: QuizControlCapabilities
      ItemizedScore: ItemizedScoreModel }

type QuizDetailsModel =
    { Code: QuizCode
      Details: Deferred<Details> }

type ListQuizStateFilter =
    | All
    | Running
    | Completed
    | Official
type ListQuizState =
    | Running
    | Completed
    | Official

type ListCompetitionStyle =
    | Individual of int
    | Team of string * string
    
type ListQuizItem =
    { Code: QuizCode
      State: ListQuizState 
      Tournament: string option
      Round: string option
      Room: string option
      CompetitionStyle: ListCompetitionStyle
      GradeDivision: GradeDivision option
      CompetitionDivision: CompetitionDivision option }

type ListQuizModel =
    { StateFilter: QuizStatusFilter
      Quizzes: Deferred<Result<ListQuizItem list, DbError>> }

//Connecting to SignalR
type HandleEventSub<'T, 'Msg> = Dispatch<'Msg> -> 'T -> Async<unit>

type ConnectAndHandleQuizEvents<'T, 'Msg> = HandleEventSub<'T, 'Msg> -> QuizCode * QuizCode option -> Sub<'Msg>

let connectAndHandleQuizEvents connectToQuiz onEvent : ConnectAndHandleQuizEvents<'T, 'Msg> =
    Unchecked.defaultof<ConnectAndHandleQuizEvents<'T, 'Msg>>
    // fun handleEvent (quizCode, previousCode) ->
    //     fun dispatch ->
    //         let connectTask = connectToQuiz quizCode previousCode
    //
    //         connectTask |> Async.Ignore |> Async.StartImmediate
    //
    //         onEvent (handleEvent dispatch)
