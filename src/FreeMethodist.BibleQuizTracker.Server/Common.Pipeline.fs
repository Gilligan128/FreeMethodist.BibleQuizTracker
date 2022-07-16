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

type TeamQuiz =
    | Unvalidated of UnvalidatedTeamQuiz
    | Running of RunningTeamQuiz
    | Completed of CompletedTeamQuiz
    | Official of OfficialTeamQuiz

type Quiz =
    | TeamQuiz of TeamQuiz
    | IndividualQuiz

//Common Dependencies
type GetTeamQuiz = QuizCode -> TeamQuiz
type SaveTeamQuiz = TeamQuiz -> unit
type PublishEventTask = string -> obj -> Async<unit>

type PublishQuizEventTask = string ->  QuizCode -> obj -> Async<unit>

type ConnectToQuizEvents = QuizCode -> Async<unit>

type NotEnoughPlayersError =
    { teamName: TeamName
      currentPlayerCount: int
      minimumPlayerCount: int }

type StartTeamQuizError = NotEnoughPlayers of NotEnoughPlayersError

module RunningTeamQuiz =
    let identity: RunningTeamQuiz =
        { Code = ""
          TeamOne =
            { Name = ""
              Score = TeamScore.identity
              Captain = None
              Quizzers = [] }
          TeamTwo =
            { Name = ""
              Score = TeamScore.identity
              Captain = None
              Quizzers = [] }
          CurrentQuestion = PositiveNumber.identity
          CurrentQuizzer = ""
          Questions = [] }

type ValidateQuizIsRunning = TeamQuiz -> Result<RunningTeamQuiz, QuizStateError>

let validateQuiz: ValidateQuizIsRunning =
    fun quiz ->
        match quiz with
        | TeamQuiz.Running running -> Ok running
        | TeamQuiz.Completed c -> Error(WrongQuizState(c.GetType()))
        | TeamQuiz.Official o -> Error(WrongQuizState(o.GetType()))
        | TeamQuiz.Unvalidated u -> Error(WrongQuizState(u.GetType()))
