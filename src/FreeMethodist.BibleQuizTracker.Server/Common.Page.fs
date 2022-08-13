module FreeMethodist.BibleQuizTracker.Server.Common_Page

open System
open System.Net
open Bolero
open Elmish
open FreeMethodist.BibleQuizTracker.Server.Capabilities
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
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

type ItemizedTeam =
    { Name: string
      Quizzers: Quizzer list }

type ItemizedCompetitionStyle =
    | Individual of Quizzer list
    | Team of ItemizedTeam * ItemizedTeam

type ItemizedScoreModel =
    { CompetitionStyle: ItemizedCompetitionStyle
      NumberOfQuestions: PositiveNumber
      QuestionsWithEvents: QuestionQuizzerEvents }

[<RequireQualifiedAccess>]
module ItemizedScoreModel =
    let refreshQuestionScore questionNumber (answerState, failedAppeal) : QuestionQuizzerEvents =
        let incorrectAnswer quizzer = (quizzer, AnsweredIncorrectly)

        let quizzerFailedAppeal failedAppeal quizzer =
            match failedAppeal with
            | None -> false
            | Some q -> q = quizzer

        let answersWithoutAppeals =
            match answerState with
            | Incomplete quizzers -> quizzers |> List.map incorrectAnswer
            | Complete (Answered question) ->
                [ (question.Answerer, AnsweredCorrectly) ]
                @ (question.IncorrectAnswerers
                   |> List.map incorrectAnswer)
            | Complete (Unanswered question) -> question |> List.map incorrectAnswer

        let answersWithAppealQuizzer =
            match failedAppeal with
            | None -> answersWithoutAppeals
            | Some quizzer when
                not
                    (
                        answersWithoutAppeals
                        |> List.map fst
                        |> List.exists (fun q -> q = quizzer)
                    )
                ->
                answersWithoutAppeals
                @ [ (quizzer, DidNotAnswer) ]
            | Some _ -> answersWithoutAppeals

        let questionQuizzerEvents =
            answersWithAppealQuizzer
            |> List.map (fun (quizzer, answer) ->
                { Position = (questionNumber, quizzer)
                  State =
                    { AnswerState = answer
                      AppealState =
                        (if quizzerFailedAppeal failedAppeal quizzer then
                             AppealFailure
                         else
                             NoFailure) } })

        questionQuizzerEvents

    let refreshQuestionScores questions =
        questions
        |> Map.map refreshQuestionScore
        |> Map.toList
        |> List.collect snd

//Quiz Details
type CompleteQuizCap = unit -> AsyncResult<CompleteQuiz.Event list, CompleteQuiz.Error>
type ReopenQuizCap =  unit -> AsyncResult<ReopenQuiz.Event list, ReopenQuiz.Error>
type Link = string

type QuizControlCapabilities = {
    CompleteQuiz : CompleteQuizCap option
    ReopenQuiz : ReopenQuizCap option
    Spectate : Link option
    LiveScore : Link option 
}
type Details =
    { State: string
      Capabilities: QuizControlCapabilities
      ItemizedScore: ItemizedScoreModel }

type QuizDetailsModel =
    { Code: QuizCode
      Details: Deferred<Details>
    } 

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
