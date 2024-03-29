﻿module FreeMethodist.BibleQuizTracker.Server.Workflow

open System
open FreeMethodist.BibleQuizTracker.Server.Tournament
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

type WithinQuizCommand<'data> = { Data: 'data; Quiz: QuizCode }

type QuizScore = private QuizScore of int //increments of 20

type TeamPosition =
    | TeamOne
    | TeamTwo

type PositiveNumber = private PositiveNumber of int

type AnsweredQuestion =
    { Answerer: Quizzer
      IncorrectAnswerers: Quizzer list }

type CompletedAnswer =
    | Answered of AnsweredQuestion
    | Unanswered of Quizzer list

type QuestionNumber = PositiveNumber

[<RequireQualifiedAccess>]
module PositiveNumber =
    let create field number =
        if number > 0 then
            Ok(PositiveNumber number)
        else
            Error $"{field} should be greater than zero"

    let one = PositiveNumber 1

    let value (PositiveNumber number) = number

    let increment (PositiveNumber i) = PositiveNumber(i + 1)
    let decrement (PositiveNumber i) = PositiveNumber(Math.Max(i - 1, 1))

    let numberOrOne number =
        create "" number
        |> function
            | Ok value -> value
            | Error _ -> one

type ParticipationState =
    | In
    | Out

type QuizzerState =
    { Name: Quizzer
      Participation: ParticipationState
      Score: QuizScore }



//don't initialize directly. Use the QuizQuestion module.
type QuizAnswer =
    | Complete of CompletedAnswer
    | Incomplete of Quizzer list

type RevertedCorrectAnswer =
    | Reverted of Quizzer
    | NoChange

type QuestionState =
    { 
      FailedAppeals: Quizzer list
      Prejumps: Quizzer list
      AnswerState: QuizAnswer }

type QuizTeamState =
    { Name: TeamName
      Score: QuizScore
      Quizzers: QuizzerState list }

type RunningCompetitionStyle =
    | Team of QuizTeamState * QuizTeamState
    | Individuals of QuizzerState list


type TeamCompetition =
    { TeamOneName: NonEmptyString
      TeamTwoName: NonEmptyString }

type CompetitionStyle =
    | Individual
    | Team of TeamCompetition


//Jumps are outside of Quizzes so that we can handle having to save a bunch around the same time.
type Jump =
    { Quiz: QuizCode
      Quizzer: Quizzer
      ServerTimestamp: DateTimeOffset }

type UnvalidatedQuiz =
    { Code: QuizCode
      TournamentInfo: TournamentInfo
      CompetitionStyle: CompetitionStyle }

type CreateTeamQuizCommand = { Data: UnvalidatedQuiz }

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


//Validation
type NoCurrentQuizzer = NoCurrentQuizzer


//Functions
[<RequireQualifiedAccess>]
module QuizScore =

    let create score =
        let scoreMod = score % 20

        if scoreMod = 0 then
            Ok(QuizScore score)
        else
            Error "Score not divisible by 20"

    let value (QuizScore score) = score

    let zero: QuizScore = QuizScore 0

    let ofQuestions questionCount = QuizScore(questionCount * 20)

    let correctAnswer (QuizScore value) = QuizScore(value + 20)
    
    let correctAnswerValue = QuizScore(20)
    
    let revertCorrectAnswer (QuizScore value) = QuizScore(value - 20)

    let failAppeal (QuizScore value) = QuizScore(value - 20)

    let revertAppealFailure (QuizScore value) = QuizScore(value + 20)
    
    let misPrejump = QuizScore(-10)
    
    let toString (QuizScore value) = string value

    let add (score1: QuizScore) (score2: QuizScore) =
        QuizScore((score1 |> value) + (score2 |> value))


[<RequireQualifiedAccess>]
module RevertedCorrectAnswer =
    let toOption revertedAnswer =
        match revertedAnswer with
        | NoChange -> None
        | Reverted q -> Some q

[<RequireQualifiedAccess>]
module QuizAnswer =
    type QuizzerAlreadyAnsweredCorrectly = QuizzerAlreadyAnsweredCorrectly of Quizzer * QuestionNumber
    type QuizzerAlreadyAnsweredIncorrectly = QuizzerAlreadyAnsweredIncorrectly of Quizzer * QuestionNumber

    let initial = Incomplete []

    let private CompletedAnswered = Answered >> Complete

    let private withoutAnswerer quizzer = List.except [ quizzer ]

    let answerCorrectly quizzer currentQuestionNumber previousQuestionState =

        match previousQuestionState with
        | Some (Complete (Answered questionState)) when quizzer = questionState.Answerer ->
            QuizzerAlreadyAnsweredCorrectly(quizzer, currentQuestionNumber)
            |> Error
        | Some (Complete (Answered questionState)) ->
            ({ questionState with
                Answerer = quizzer
                IncorrectAnswerers =
                    questionState.IncorrectAnswerers
                    @ [ questionState.Answerer ] }
             |> CompletedAnswered,
             Reverted questionState.Answerer)
            |> Ok
        | Some (Complete (Unanswered questionState)) ->
            ({ Answerer = quizzer
               IncorrectAnswerers = questionState |> withoutAnswerer quizzer }
             |> Answered
             |> Complete,
             NoChange)
            |> Ok
        | Some (Incomplete answerers) ->
            (CompletedAnswered
                { Answerer = quizzer
                  IncorrectAnswerers = answerers },
             NoChange)
            |> Ok
        | None ->
            ({ Answerer = quizzer
               IncorrectAnswerers = [] }
             |> CompletedAnswered,
             NoChange)
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
             Reverted quizzer)
            |> Ok
        | Some (Complete (Answered answeredQuestion)) when
            answeredQuestion.IncorrectAnswerers
            |> List.contains (quizzer)
            ->
            Error(QuizzerAlreadyAnsweredIncorrectly(quizzer, currentQuestionNumber))
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

[<RequireQualifiedAccess>]
module QuestionState =
    let initial =
        { Prejumps = []
          FailedAppeals = []
          AnswerState = QuizAnswer.initial }

    let create answer = { initial with AnswerState = answer }

    let changeAnswer answer questionStateOption =
        questionStateOption
        |> Option.defaultValue initial
        |> fun q -> Some { q with AnswerState = answer }

    let failAppeal quizzer question =
        { question with FailedAppeals = question.FailedAppeals |> List.append [quizzer] }


[<RequireQualifiedAccess>]
module QuizzerState =
    let create name =
        { Name = name
          Participation = In
          Score = QuizScore.zero }

    let isQuizzer quizzerName (quizzerState: QuizzerState) = quizzerState.Name = quizzerName

    let updateQuizzerIfInRoster
        (updateFunction: QuizzerState -> QuizzerState)
        quizzerName
        (quizzerRoster: QuizzerState list)
        =
        quizzerRoster
        |> List.map (fun q ->
            if isQuizzer quizzerName q then
                updateFunction q
            else
                q)


[<RequireQualifiedAccess>]
module QuizTeamState =
    let updateQuizzerIfFound updateFunction quizzer (team: QuizTeamState) =
        { team with
            Quizzers =
                team.Quizzers
                |> QuizzerState.updateQuizzerIfInRoster updateFunction quizzer }


type DbError =
    | Exception of exn
    | RemoteError of string

