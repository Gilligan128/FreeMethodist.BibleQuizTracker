module FreeMethodist.BibleQuizTracker.Server.QuizzingDomain

open System
open System.ComponentModel.DataAnnotations
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open Microsoft.FSharp.Core
open QuizzingApi

//We will eventually modularize this into separate files, probbaly by Workflow.

//Domain Model



type QuizRoomState =
    | Open
    | Closed


type CompletedQuizzer = {
    Name: Quizzer
    Score: int
}

type CompletedTeam = {
    Name: TeamName
    Score: int
    Quizzers: Quizzer list
}

type CompletedTeamQuiz =
    { code: QuizCode
      winningTeam: CompletedTeam
      losingTeam: CompletedTeam
      completedQuestions: CompletedQuestion list }

type OfficialQuizzer = {
    Name: Quizzer
    Score: TeamScore
}
type OfficialTeam = {
    Name: TeamName
    Score: TeamScore
    QuizzerOne: OfficialQuizzer
    QuizzerTwo: OfficialQuizzer
    QuizzerThree: OfficialQuizzer option
    QuizzerFour: OfficialQuizzer option
    QuizzerFive: OfficialQuizzer option
}
type OfficialTeamQuiz = {
    Code: QuizCode
    WinningTeam: OfficialTeam
    LosingTeam: OfficialTeam
    CompletedQuestions: CompletedQuestion list 
}

type TeamQuiz =
    | Unvalidated of UnvalidatedTeamQuiz
    | Running of RunningTeamQuiz
    | Completed of CompletedTeamQuiz
    | Official of OfficialTeamQuiz

type Quiz =
    | TeamQuiz of TeamQuiz
    | IndividualQuiz


//Common Dependencies
type GetQuiz = QuizCode -> Quiz




type NotEnoughPlayersError =
    { teamName: TeamName
      currentPlayerCount: int
      minimumPlayerCount: int }

type StartTeamQuizError = NotEnoughPlayers of NotEnoughPlayersError
