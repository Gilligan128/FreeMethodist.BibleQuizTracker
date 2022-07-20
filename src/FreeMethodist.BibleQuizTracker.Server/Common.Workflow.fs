module FreeMethodist.BibleQuizTracker.Server.Workflow

open System
open Microsoft.FSharp.Core

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

    let initial: TeamScore = TeamScore 0

    let ofQuestions questionCount = TeamScore(questionCount * 20)

    let correctAnswer (TeamScore value) = TeamScore(value + 20)
    let revertCorrectAnswer (TeamScore value) = TeamScore(value - 20)

type TeamPosition =
    | TeamOne
    | TeamTwo

type PositiveNumber = private PositiveNumber of int


type QuestionNumber = PositiveNumber

[<RequireQualifiedAccess>]
module PositiveNumber =
    let create number field =
        if number > 0 then
            Ok(PositiveNumber number)
        else
            Error $"{field} should be greater than zero"

    let one = PositiveNumber 1

    let value (PositiveNumber number) = number

    let increment (PositiveNumber i) = PositiveNumber(i + 1)
    let decrement (PositiveNumber i) = PositiveNumber(Math.Max(i - 1, 1))

type ParticipationState =
    | In
    | Out

type AnsweredQuestion =
    { Answerer: Quizzer
      IncorrectAnswerers: Quizzer list }

type CompletedQuestion =
    | Answered of AnsweredQuestion
    | Unanswered of Quizzer list

type QuizzerState =
    { Name: Quizzer
      Participation: ParticipationState
      Score: TeamScore }

module QuizzerState =
    let create name =
        { Name = name
          Participation = In
          Score = TeamScore.initial }

//don't initialize directly. Use the QuizQuestion module.
type QuizQuestion =
    | Complete of CompletedQuestion
    | Incomplete of Quizzer list
type RevertedCorrectAnswer =
    | Reverted
    | NoChange
    
[<RequireQualifiedAccess>]
module QuizQuestion =
    type QuizzerAlreadyAnsweredCorrectly = QuizzerAlreadyAnsweredCorrectly of Quizzer * QuestionNumber
    type QuizzerAlreadyAnsweredIncorrectly = QuizzerAlreadyAnsweredIncorrectly of Quizzer * QuestionNumber

    
    let create = Incomplete []

    let private CompletedAnswered =
        Answered >> Complete

    let private withoutAnswerer quizzer = List.except [ quizzer ]

    let answerCorrectly quizzer currentQuestionNumber previousQuestionState =

        match previousQuestionState with
        | Some (Complete (Answered questionState)) when quizzer = questionState.Answerer ->
            QuizzerAlreadyAnsweredCorrectly(quizzer, currentQuestionNumber)
            |> Error
        | Some (Complete (Answered questionState)) -> questionState |> CompletedAnswered |> Ok
        | Some (Complete (Unanswered questionState)) ->
            { Answerer = quizzer
              IncorrectAnswerers = questionState |> withoutAnswerer quizzer }
            |> Answered
            |> Complete
            |> Ok
        | Some (Incomplete answerers) ->
            CompletedAnswered
                { Answerer = quizzer
                  IncorrectAnswerers = answerers }
            |> Ok
        | None ->
            { Answerer = quizzer
              IncorrectAnswerers = [] }
            |> CompletedAnswered
            |> Ok
            
    let answerIncorrectly quizzer currentQuestionNumber questionOpt =
         let addQuizzerDistinct quizzer quizzers = quizzers @ [ quizzer ] |> List.distinct
         
         match questionOpt with
         | None -> (Incomplete [ quizzer ], NoChange) |> Ok
         | Some (Incomplete quizzers) ->
            (Incomplete(quizzers |> addQuizzerDistinct quizzer), NoChange)
            |> Ok
         | Some (Complete (Answered answeredQuestion)) when quizzer = answeredQuestion.Answerer ->
            (answeredQuestion.IncorrectAnswerers
             |> addQuizzerDistinct quizzer
             |> Unanswered
             |> Complete,
             Reverted)
            |> Ok
         | Some (Complete (Answered answeredQuestion)) when
            answeredQuestion.IncorrectAnswerers
            |> List.contains (quizzer)
            ->
            Error(QuizzerAlreadyAnsweredIncorrectly(quizzer, currentQuestionNumber)
            )
         | Some (Complete (Answered answeredQuestion)) ->
            ({ answeredQuestion with
                IncorrectAnswerers =
                    answeredQuestion.IncorrectAnswerers
                    |> addQuizzerDistinct quizzer }
             |> Answered
             |> Complete,
             NoChange)
            |> Ok
         | Some (Complete (Unanswered question)) when question |> List.contains (quizzer) ->
            Error(QuizzerAlreadyAnsweredIncorrectly(quizzer, currentQuestionNumber))
         | Some (Complete (Unanswered question)) ->
            (question
             |> addQuizzerDistinct quizzer
             |> (Unanswered >> Complete),
             NoChange)
            |> Ok

        

type QuizTeamState =
    { Name: TeamName
      Score: TeamScore
      Quizzers: QuizzerState list
      Captain: Quizzer option }

type RunningTeamQuiz =
    { Code: QuizCode
      Questions: Map<PositiveNumber, QuizQuestion>
      TeamOne: QuizTeamState
      TeamTwo: QuizTeamState
      CurrentQuestion: QuestionNumber
      CurrentQuizzer: Quizzer option }

//Jumps are outside of Quizzes so that we can handle having to save a bunch around the same time.
type Jump =
    { Quiz: QuizCode
      Quizzer: Quizzer
      ServerTimestamp: DateTimeOffset }

//Create Quiz Workflow
type UnvalidatedTeamQuiz =
    { Code: QuizCode
      TeamOne: TeamName
      TeamTwo: TeamName }

type CreateTeamQuizCommand = { Data: UnvalidatedTeamQuiz }

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
    let identity =
        { Code = "Example"
          TeamOne =
            { Name = "LEFT"
              Score = TeamScore.initial
              Captain = None
              Quizzers = [] }
          TeamTwo =
            { Name = "RIGHT"
              Score = TeamScore.initial
              Captain = None
              Quizzers = [] }
          CurrentQuestion = PositiveNumber.one
          CurrentQuizzer = None
          Questions = Map.empty }
