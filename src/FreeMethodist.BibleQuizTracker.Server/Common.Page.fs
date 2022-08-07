module FreeMethodist.BibleQuizTracker.Server.Common_Page

open System
open System.Net
open Bolero
open Elmish
open FreeMethodist.BibleQuizTracker.Server.Workflow

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


let mapDbErrorToString error =
    match error with
    | Exception exn -> exn.Message
    | DbError.RemoteError message -> message

let mapQuizStateErrorToString _ = "Wrong quiz state"

//Itemized Score model
type EventState =
    { AnswerState: AnswerState
      AppealState: AppealState }

type EventPosition = QuestionNumber * Quizzer

type QuestionQuizzerEvent =
    { Position: EventPosition
      State: EventState }

type QuestionQuizzerEvents = QuestionQuizzerEvent list

type ItemizedScoreModel =
    { TeamOne: TeamModel
      TeamTwo: TeamModel
      QuestionsBetter: QuestionQuizzerEvents
      Questions: Map<Quizzer, AnswerState * AppealState> list }

//Live Score model


//Quiz Details
type Details = { ItemizedScore: ItemizedScoreModel }


type QuizDetailsModel =
    { Code: QuizCode
      Details: Deferred<Details> }




//Connecting to SignalR
type HandleEventSub<'T, 'Msg> = Dispatch<'Msg> -> 'T -> Async<unit>

type ConnectAndHandleQuizEvents<'T, 'Msg> = HandleEventSub<'T, 'Msg> -> QuizCode * QuizCode option -> Sub<'Msg>

let connectAndHandleQuizEvents connectToQuiz onEvent : ConnectAndHandleQuizEvents<'T, 'Msg> =
    fun handleEvent (quizCode, previousCode) ->
        fun dispatch ->
            let connectTask =
                connectToQuiz quizCode previousCode

            connectTask
            |> Async.Ignore
            |> Async.StartImmediate

            onEvent (handleEvent dispatch)
