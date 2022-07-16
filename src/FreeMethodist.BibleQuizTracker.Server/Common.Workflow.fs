module FreeMethodist.BibleQuizTracker.Server.Workflow

open System
open Microsoft.FSharp.Core

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
    
    let identity: TeamScore  = TeamScore 0
    
    let ofQuestions questionCount =
        TeamScore (questionCount * 20)
type TeamPosition =
    | TeamOne
    | TeamTwo

type PositiveNumber = PositiveNumber of int

type QuestionNumber = PositiveNumber
[<RequireQualifiedAccess>]
module PositiveNumber =
    let create number field =
        if number > 0 then Ok (PositiveNumber number) else Error $"{field} should be greater than zero"
    
    let identity = PositiveNumber 1
    
    let value (PositiveNumber number) = number

 
//Create Quiz Workflow
type UnvalidatedTeamQuiz =
    { Code: QuizCode
      TeamOne: TeamName
      TeamTwo: TeamName }

type CreateTeamQuizCommand = { Data: UnvalidatedTeamQuiz }


type CompletedQuestion = { answeringPlayer: Quizzer option }

type ParticipationState =
    | In
    | Out

type QuizzerState =
    { Name: Quizzer
      Participation: ParticipationState
      Score: TeamScore }

type QuizQuestion =
    | Completed of CompletedQuestion
    | Upcoming

type QuizTeamState =
    { Name: TeamName
      Score: TeamScore
      Quizzers: QuizzerState list
      Captain: Quizzer option }

type RunningTeamQuiz =
    { Code: QuizCode
      Questions: QuizQuestion list
      TeamOne: QuizTeamState
      TeamTwo: QuizTeamState
      CurrentQuestion: QuestionNumber
      CurrentQuizzer: Quizzer }

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

type QuizStateError = WrongQuizState of Type

[<RequireQualifiedAccess>]
module RunningTeamQuiz =
    let identity = { Code = "Example"
                     TeamOne =
                        { Name = "LEFT"
                          Score = TeamScore.identity 
                          Captain = None
                          Quizzers =  [ ] }
                     TeamTwo =
                        { Name = "RIGHT"
                          Score = TeamScore.identity
                          Captain = None
                          Quizzers = [ ] }
                     CurrentQuestion = PositiveNumber.identity
                     CurrentQuizzer = ""
                     Questions = [] }