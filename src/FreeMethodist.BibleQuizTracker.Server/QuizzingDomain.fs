module FreeMethodist.BibleQuizTracker.Server.QuizzingDomain

open System
open System.ComponentModel.DataAnnotations
open Microsoft.FSharp.Core
open QuizzingApi

//We will eventually modularize this into separate files, probbaly by Workflow.

//Domain Model

type TeamScore = int //increments of 20


type QuestionNumber = int

type ParticipationState =
    | In
    | Out

type QuizRoomState =
    | Open
    | Closed

type CompletedQuestion = { answeringPlayer: Quizzer option }

type QuizQuestion =
    | Completed of CompletedQuestion
    | Upcoming

type CompletedQuiz =
    { code: QuizCode
      winningTeam: TeamName
      losingTeam: TeamName
      completedQuestions: CompletedQuestion list }

type PlayerQuizState =
    { player: Quizzer
      participation: ParticipationState
      score: TeamScore }

type QuizTeamState =
    { name: TeamName
      quizzerOne: PlayerQuizState
      quizzerTwo: PlayerQuizState
      quizzerThree: PlayerQuizState option
      substituteOne: PlayerQuizState option
      substituteTwo: PlayerQuizState option
      captain: Quizzer }

type RunningQuiz =
    { code: QuizCode
      questions: QuizQuestion list
      teamOne: QuizTeamState
      teamTwo: QuizTeamState }

type TeamQuiz =
    | Unvalidated of UnvalidatedQuiz
    | Preparing of PreparingTeamQuiz
    | Running of RunningQuiz
    | Completed of CompletedQuiz

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
