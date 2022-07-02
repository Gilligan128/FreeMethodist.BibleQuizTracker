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
type UnvalidatedTeamQuiz =
    { Code: QuizCode
      TeamOne: TeamName
      TeamTwo: TeamName }

type CreateTeamQuizCommand = { Data: UnvalidatedTeamQuiz }

type TeamScore = private TeamScore of int //increments of 20

[<RequireQualifiedAccess>]
module TeamScore =

    let create score =
        let scoreMod = score % 20

        if scoreMod = 0 then
            Ok(TeamScore score)
        else
            Error "Score not divisible by 20"

    let value (TeamScore score) = score

type QuestionNumber = int

type CompletedQuestion = { answeringPlayer: Quizzer option }

type ParticipationState =
    | In
    | Out

type QuizzerState =
    { Player: Quizzer
      Participation: ParticipationState
      Score: TeamScore }

type QuizQuestion =
    | Completed of CompletedQuestion
    | Upcoming

type QuizTeamState =
    { Name: TeamName
      Quizzers: QuizzerState list
      captain: Quizzer option }

type RunningTeamQuiz =
    { code: QuizCode
      questions: QuizQuestion list
      TeamOne: QuizTeamState
      TeamTwo: QuizTeamState }

type TeamQuizCreated = { Quiz: RunningTeamQuiz }

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
    | OverrideTeamScore
    | MarkQuizzerIn
    | MarkQuizzerOut
    | SubstituteQuizzer
    | MakeCaptain
    | CompleteQuiz
    | EnterQuiz of EnterTeamQuizCommand
