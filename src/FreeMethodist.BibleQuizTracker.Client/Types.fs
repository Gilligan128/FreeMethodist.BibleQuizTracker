module FreeMethodist.BibleQuizTracker.Client.Types

open System
open Microsoft.FSharp.Core

type NonEmptyString = string

type Quizzer = NonEmptyString

type TeamName = NonEmptyString

type TeamScore = int //increments of 20

type QuizCode = NonEmptyString

type QuestionNumber = int

type User =
    | Quizmaster
    | Scorekeeper
    | Quizzer of Quizzer
    | Spectator

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

type PreparingTeamQuiz =
    { code: QuizCode
      captain: Quizzer option
      quizzerTwo: Quizzer option
      quizzerThree: Quizzer option }

type TeamQuiz =
    | Completed of CompletedQuiz
    | Running of RunningQuiz
    | Preparing of PreparingTeamQuiz

type Quiz =
    | TeamQuiz of TeamQuiz
    | IndividualQuiz

type RunTeamQuizCommand =
    | CorrectAnswer of Quizzer
    | IncorrectAnswer of Quizzer
    | MarkPrejump of Quizzer
    | Jump of Quizzer * DateTimeOffset
    | ResetQuestion
    | NextQuestion
    | SelectQuestion of QuestionNumber
    | LockJumps
    | ClearJumps
    | OverrideScore of Quizzer * TeamScore
    | MarkQuizzerIn of Quizzer
    | MarkQuizzerOut of Quizzer
    | SubstituteQuizzer of Quizzer * Quizzer
    | MakeCaptain of Quizzer
    | CompleteQuiz

type RunTeamQuizError = QuizzerNotParticipating of Quizzer

type WithinQuizCommand =
    { code: QuizCode
      runCommand: RunTeamQuizCommand }

type CreateTeamQuizCommand = QuizCode * TeamName * TeamName

type CreateTeamQuizError =
    | QuizWithThatCodeAlreadyExists of QuizCode
    | TeamNamesAreSame of TeamName

type NotEnoughPlayersError =
    { teamName: TeamName
      currentPlayerCount: int
      minimumPlayerCount: int }

type StartTeamQuizError = NotEnoughPlayers of NotEnoughPlayersError

type ParticipationType =
    | Captain
    | Active
    | Substitute

type EnterQuizCommand = Quizzer * TeamName * QuizCode * ParticipationType
type EnterQuizError = AlreadyACaptain of Quizzer

type CreateTeamQuiz = QuizCode list -> CreateTeamQuizCommand -> Result<PreparingTeamQuiz, CreateTeamQuizError>
type EnterQuiz = Quiz option -> EnterQuizCommand -> Result<TeamQuiz, EnterQuizError>
type RunTeamQuiz = RunningQuiz -> RunTeamQuizCommand -> Result<TeamQuiz, RunTeamQuizError>
type StartTeamQuiz = PreparingTeamQuiz -> Result<RunningQuiz, StartTeamQuizError>
