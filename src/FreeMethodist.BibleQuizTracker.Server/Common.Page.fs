module FreeMethodist.BibleQuizTracker.Server.Common_Page

open System
open Bolero
open Elmish
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.AspNetCore.Http

type AsyncOperationStatus<'started, 'finished> =
    | Started of 'started
    | Finished of 'finished

type Deferred<'T> =
    | NotYetLoaded
    | InProgress
    | Loaded of 'T
    
//Live Score model
type LiveScoreQuizzer = { Score : TeamScore; Name : Quizzer }
type LiveScoreIndividuals = { Quizzers : LiveScoreQuizzer list }
type LiveScoreTeam = { Name : string; Score : TeamScore; Quizzers: LiveScoreQuizzer list  }
type LiveScoreCompetitionStyle =
    | Individual of LiveScoreIndividuals
    | Team of LiveScoreTeam*LiveScoreTeam
type LiveScores = { LastUpdated : DateTimeOffset; CurrentQuestion: QuestionNumber; CompetitionStyle : LiveScoreCompetitionStyle }
type LiveScoreModel = { Code : QuizCode; Scores: Deferred<LiveScores>  }

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/quiz/{quizCode}/run">] QuizRun of quizCode: string
    | [<EndPoint "/quiz/{quizCode}/spectate">] QuizSpectate of quizCode: string
    | [<EndPoint "/quiz/{quizCode}/live-score">] QuizLiveScore of quizCode: string*PageModel<LiveScoreModel>

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

//Scoring types
type EventState = { AnswerState : QuizAnswer; AppealState : AppealState }

type EventPosition = QuestionNumber*Quizzer

type QuestionQuizzerEvent = { Position : EventPosition; State: EventState }
type QuestionQuizzerEvents = QuestionQuizzerEvent list

//Connecting to SignalR
type HandleEventSub<'T,'Msg> = Dispatch<'Msg> -> 'T -> Async<unit>

type ConnectAndHandleQuizEvents<'T, 'Msg> = HandleEventSub<'T,'Msg> -> QuizCode*QuizCode option -> Sub<'Msg>

let connectAndHandleQuizEvents connectToQuiz onEvent : ConnectAndHandleQuizEvents<'T, 'Msg> =
    fun handleEvent (quizCode, previousCode) ->
       fun dispatch ->
           let connectTask = connectToQuiz quizCode previousCode
           connectTask
           |> Async.Ignore
           |> Async.StartImmediate
           onEvent (handleEvent dispatch)
      
