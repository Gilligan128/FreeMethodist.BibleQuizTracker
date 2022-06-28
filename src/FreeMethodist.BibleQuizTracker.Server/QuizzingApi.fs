module FreeMethodist.BibleQuizTracker.Server.QuizzingApi

open System

//Common
type NonEmptyString = string

type QuizCode = NonEmptyString

type Quizzer = NonEmptyString

type User =
    | Quizmaster
    | Scorekeeper
    | Quizzer of Quizzer
    | Spectator

type TeamName = NonEmptyString

type WithinQuizCommand<'data> =
    { Data: 'data
      Quiz: QuizCode
      User: User }
    
//Create Quiz Workflow
type UnvalidatedQuiz =
    { Code: QuizCode
      TeamOne: TeamName
      TeamTwo: TeamName }
type CreateTeamQuizCommand = {
  Data: UnvalidatedQuiz
}
type PreparingTeam =
    { Name: TeamName
      QuizzerOne: Quizzer option
      QuizzerTwo: Quizzer option
      QuizzerThree: Quizzer option
      SubstituteOne: Quizzer option
      SubstituteTwo: Quizzer option
      Captain: Quizzer option }

type PreparingTeamQuiz =
    { Code: QuizCode
      TeamOne: PreparingTeam
      TeamTwo: PreparingTeam }

type TeamQuizCreated = {
    Quiz: PreparingTeamQuiz
}
type CreateTeamQuizError =
    | QuizWithThatCodeAlreadyExists of QuizCode
    | TeamNamesAreSame of TeamName

type CreateTeamQuizWorkflow = CreateTeamQuizCommand -> Result<TeamQuizCreated, CreateTeamQuizError>

//Enter Quiz Workflow
type ParticipationType =
    | Captain
    | Active
    | Substitute

type UnvalidatedEntrance =
    { Quizzer: Quizzer
      Team: TeamName
      Participation: ParticipationType
      Timestamp: DateTimeOffset }

type EnterTeamQuizCommand = WithinQuizCommand<UnvalidatedEntrance>

type EnterQuizError =
    | ThereIsAlreadyACaptain of Quizzer
    | QuizNotFound of QuizCode
    | QuizNotTeamQuiz of QuizCode
    | QuizIsCompleted of QuizCode

type QuizzerEntered =
    { Quizzer: Quizzer
      Quiz: QuizCode
      Timestamp: DateTimeOffset }

type EnterQuizWorkflow = EnterTeamQuizCommand -> Result<QuizzerEntered, EnterQuizError>

//Jump workflow
type UnvalidatedJump =
    { Quizzer: Quizzer
      timestamp: DateTimeOffset }

type JumpCommand = WithinQuizCommand<UnvalidatedJump>
type JumpOrderChanged = { Quiz: QuizCode; Order: Quizzer list }

type JumpError =
    | JumpingLocked
    | QuizzerNotParticipating of Quizzer
    | QuizRoomClosed of QuizCode

type PlayerJumpsWorkflow = JumpCommand -> Result<JumpOrderChanged, JumpError>

//Full documentation for commands while quiz is running.
//This is sort of a "to do list" of API
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
    | EnterQuiz of EnterTeamQuizCommand
