module FreeMethodist.BibleQuizTracker.Server.Pipeline

open System
open System.ComponentModel.DataAnnotations
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core
open Workflow

//We will eventually modularize this into separate files, probbaly by Workflow.

//Domain Model


type QuizRoomState =
    | Open
    | Closed


type CompletedQuizzer = { Name: Quizzer; Score: int }

type CompletedTeam =
    { Name: TeamName
      Score: int
      Quizzers: Quizzer list }

type CompletedTeamQuiz =
    { code: QuizCode
      winningTeam: CompletedTeam
      losingTeam: CompletedTeam
      completedQuestions: CompletedQuestion list }

type OfficialQuizzer = { Name: Quizzer; Score: TeamScore }

type OfficialTeam =
    { Name: TeamName
      Score: TeamScore
      QuizzerOne: OfficialQuizzer
      QuizzerTwo: OfficialQuizzer
      QuizzerThree: OfficialQuizzer option
      QuizzerFour: OfficialQuizzer option
      QuizzerFive: OfficialQuizzer option }

type OfficialTeamQuiz =
    { Code: QuizCode
      WinningTeam: OfficialTeam
      LosingTeam: OfficialTeam
      CompletedQuestions: CompletedQuestion list }

type Quiz =
    | Unvalidated of UnvalidatedTeamQuiz
    | Running of RunningTeamQuiz
    | Completed of CompletedTeamQuiz
    | Official of OfficialTeamQuiz

//Common Dependencies
type GetTeamQuiz = QuizCode -> Quiz
type GetTeamQuizAsync = QuizCode -> Async<Quiz>
type SaveTeamQuiz = Quiz -> unit
type SaveTeamQuizAsync = Quiz -> Async<unit>

type PublishQuizEventTask = string ->  QuizCode -> obj -> Async<unit>


type GetJumps = QuizCode -> Jump seq
type SaveJump = Jump -> unit

type ConnectToQuizEvents = QuizCode -> QuizCode option -> Async<unit>

type NotEnoughPlayersError =
    { teamName: TeamName
      currentPlayerCount: int
      minimumPlayerCount: int }

type StartTeamQuizError = NotEnoughPlayers of NotEnoughPlayersError
type ValidateQuizIsRunning = Quiz -> Result<RunningTeamQuiz, QuizStateError>

let validateQuiz: ValidateQuizIsRunning =
    fun quiz ->
        match quiz with
        | Quiz.Running running -> Ok running
        | Quiz.Completed c -> Error(WrongQuizState(c.GetType()))
        | Quiz.Official o -> Error(WrongQuizState(o.GetType()))
        | Quiz.Unvalidated u -> Error(WrongQuizState(u.GetType()))
