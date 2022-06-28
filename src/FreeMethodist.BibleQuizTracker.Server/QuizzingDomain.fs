module FreeMethodist.BibleQuizTracker.Server.QuizzingDomain

open System
open System.ComponentModel.DataAnnotations
open Microsoft.FSharp.Core

//We will eventually modularize this into separate files, probbaly by Workflow.

//Domain Model
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

//API
type WithinQuizCommand<'data> = {
    Data: 'data
    Quiz: QuizCode
    User: User
}

//Common Dependencies
type GetQuiz = QuizCode -> Quiz 

//Jump workflow
type UnvalidatedJump =  {
      Quizzer: Quizzer
      timestamp: DateTimeOffset }
type JumpCommand = WithinQuizCommand<UnvalidatedJump>
type JumpOrderChanged = {
    Quiz: QuizCode
    Order: Quizzer list
    }
type JumpError =
    | JumpingLocked
    | QuizzerNotParticipating of Quizzer
    | QuizRoomClosed of QuizCode
//Jump API    
type PlayerJumpsWorkflow = JumpCommand -> Result<JumpOrderChanged, JumpError>

//Jump pipeline
type ValidJump = {
    Quizzer: Quizzer
    Timestamp: DateTimeOffset
}

type Jump =
    | UnvalidatedJump
    | ValidJump

//Jump Pipeline steps
type ValidateJump = JumpCommand -> Result<ValidJump, JumpError>
type GetExistingJumps = QuizCode -> ValidJump list  
type CalculateJumpOrder = ValidJump list -> QuizCode -> ValidJump -> JumpOrderChanged

type RunTeamQuizCommands =
    | CorrectAnswer
    | IncorrectAnswer 
    | MarkPrejump 
    | Jump of JumpCommand
    | ResetQuestion
    | NextQuestion
    | SelectQuestion 
    | LockJumps
    | ClearJumps
    | OverrideScore 
    | MarkQuizzerIn 
    | MarkQuizzerOut 
    | SubstituteQuizzer
    | MakeCaptain 
    | CompleteQuiz

type CreateTeamQuizCommand = QuizCode * TeamName * TeamName
type CreateTeamQuizError =
    | QuizWithThatCodeAlreadyExists of QuizCode
    | TeamNamesAreSame of TeamName

type NotEnoughPlayersError =
    { teamName: TeamName
      currentPlayerCount: int
      minimumPlayerCount: int }

type StartTeamQuizError = NotEnoughPlayers of NotEnoughPlayersError


//Enter Quiz Workflow
type ParticipationType =
    | Captain
    | Active
    | Substitute
type UnvalidatedEntrance = {
    Quizzer: Quizzer
    Team: TeamName
    Participation: ParticipationType
    Timestamp: DateTimeOffset
}
type EnterTeamQuizCommand = WithinQuizCommand<UnvalidatedEntrance>
type EnterQuizError =
    | ThereIsAlreadyACaptain of Quizzer
    | QuizNotFound of QuizCode
    | QuizNotTeamQuiz of QuizCode
type QuizzerEntered = {
    Quizzer: Quizzer
    Quiz: QuizCode
    Timestamp: DateTimeOffset
}
type EnterQuizWorkflow = EnterTeamQuizCommand -> Result<QuizzerEntered, EnterQuizError>

//Enter Quiz Pipeline
type ValidEntrance = {
    Quizzer: Quizzer
    Team: TeamName
    Participation: ParticipationType
    Timestamp: DateTimeOffset
}
type ValidateEntrance = QuizCode -> UnvalidatedEntrance -> Result<ValidEntrance, EnterQuizError>
type ReportEntrance = QuizCode -> ValidEntrance -> QuizzerEntered

type CreateTeamQuiz = CreateTeamQuizCommand -> Result<PreparingTeamQuiz, CreateTeamQuizError>
