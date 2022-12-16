module FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core
open Elmish


//Domain Model

type QuizRoomState =
    | Open
    | Closed

type CompletedQuizzer = { Name: Quizzer; Score: QuizScore }

type CompletedTeam =
    { Name: TeamName
      Score: QuizScore
      Quizzers: CompletedQuizzer list }

type CompletedCompetitionStyle =
    | Individual of CompletedQuizzer list
    | Team of CompletedTeam * CompletedTeam

type CompletedQuestion =
    { Prejumps: Quizzer list
      FailedAppeals: Quizzer list
      AnswerState: CompletedAnswer }

type CompletedQuiz =
    { Code: QuizCode
      CompetitionStyle: CompletedCompetitionStyle
      CompletedQuestions: CompletedQuestion list }

type OfficialTeam =
    { Name: TeamName
      Score: QuizScore
      QuizzerOne: CompletedQuizzer
      QuizzerTwo: CompletedQuizzer
      QuizzerThree: CompletedQuizzer option
      QuizzerFour: CompletedQuizzer option
      QuizzerFive: CompletedQuizzer option }

[<RequireQualifiedAccess>]
module OfficialTeam =
    let quizzerList (team: OfficialTeam) =
        [ Some team.QuizzerOne
          Some team.QuizzerTwo
          team.QuizzerThree
          team.QuizzerFour
          team.QuizzerFive ]

type OfficialCompetitionStyle =
    | Individual of CompletedQuizzer list
    | Team of OfficialTeam * OfficialTeam

type OfficialTeamQuiz =
    { Code: QuizCode
      WinningTeam: OfficialTeam
      LosingTeam: OfficialTeam
      CompetitionStyle: OfficialCompetitionStyle
      CompletedQuestions: CompletedQuestion list }

type Quiz =
    | Running of RunningQuiz
    | Completed of CompletedQuiz
    | Official of OfficialTeamQuiz

[<RequireQualifiedAccess>]
module Quiz =
    let start (unvalidatedQuiz: UnvalidatedQuiz) =
        match unvalidatedQuiz.CompetitionStyle with
        | CompetitionStyle.Individual ->
             Running
                { Code = unvalidatedQuiz.Code
                  CompetitionStyle =
                    RunningCompetitionStyle.Individuals []
                  CurrentQuizzer = None
                  CurrentQuestion = PositiveNumber.one
                  Questions =
                    Map.empty
                    |> Map.add PositiveNumber.one QuestionState.initial }
        | CompetitionStyle.Team teams ->
            Running
                { Code = unvalidatedQuiz.Code
                  CompetitionStyle =
                    RunningCompetitionStyle.Team(
                        { Name = teams.TeamOneName
                          Score = QuizScore.zero
                          Quizzers = [] },
                        { Name = teams.TeamTwoName
                          Score = QuizScore.zero
                          Quizzers = [] }
                    )
                  CurrentQuizzer = None
                  CurrentQuestion = PositiveNumber.one
                  Questions =
                    Map.empty
                    |> Map.add PositiveNumber.one QuestionState.initial }
       |> Ok

    let getCode quiz =
        match quiz with
        | Running r -> r.Code
        | Completed completedQuiz -> completedQuiz.Code
        | Official officialTeamQuiz -> officialTeamQuiz.Code
//Persistence
type GetQuiz = QuizCode -> AsyncResult<Quiz, DbError>
type SaveQuiz = Quiz -> AsyncResult<unit, DbError>
type SaveNewQuiz = SaveNewQuiz of SaveQuiz
type CheckQuizCodeExists = QuizCode -> AsyncResult<Option<string>, string>

type GetJumps = QuizCode -> Jump seq
type SaveJump = Jump -> unit

type TryGetQuiz = QuizCode -> AsyncResult<Option<Quiz>, DbError>

type GetRecentCompletedQuizzes = unit -> AsyncResult<string list, DbError>

//Event Handling
type PublishQuizEventTask = string -> QuizCode -> obj -> Async<unit>

type ConnectToQuizEvents = QuizCode -> QuizCode option -> Async<unit>
type ListenToRunQuizEvents<'Msg> = Dispatch<'Msg> -> unit

type NotEnoughPlayersError =
    { teamName: TeamName
      currentPlayerCount: int
      minimumPlayerCount: int }

type StartTeamQuizError = NotEnoughPlayers of NotEnoughPlayersError

//Validation
type ValidateQuizIsRunning = Quiz -> Result<RunningQuiz, QuizStateError>
type ValidateCurrentQuizzer = RunningQuiz -> Result<Quizzer, NoCurrentQuizzer>
type ValidateCurrentQuizzerWithTeam = RunningQuiz -> Result<Quizzer * TeamPosition option, NoCurrentQuizzer>

let validateCurrentQuizzer: ValidateCurrentQuizzer =
    fun quiz ->
        quiz.CurrentQuizzer
        |> (Result.ofOption NoCurrentQuizzer)

let validateCurrentQuizzerWithTeam: ValidateCurrentQuizzerWithTeam =
    fun quiz ->
        quiz.CurrentQuizzer
        |> Option.bind (fun quizzer -> quizzer |> RunningQuiz.tryFindQuizzer quiz)
        |> Option.map (fun (quizzer, team) -> quizzer.Name, team)
        |> Result.ofOption NoCurrentQuizzer

let validateCompleteQuiz quiz =
    match quiz with
    | Quiz.Running running -> Error(WrongQuizState(running.GetType()))
    | Quiz.Completed c -> c |> Choice1Of2 |> Ok
    | Quiz.Official official -> official |> Choice2Of2 |> Ok

let validateRunningQuiz: ValidateQuizIsRunning =
    fun quiz ->
        match quiz with
        | Quiz.Running running -> Ok running
        | Quiz.Completed c -> Error(WrongQuizState(c.GetType()))
        | Quiz.Official o -> Error(WrongQuizState(o.GetType()))

//Changing current question
let changeCurrentQuestionInQuiz question quiz =
    { quiz with
        CurrentQuestion = question
        Questions =
            quiz.Questions
            |> Map.change quiz.CurrentQuestion (fun q ->
                q
                |> Option.defaultValue (QuestionState.create ([] |> Unanswered |> Complete))
                |> Some)
            |> Map.change question (fun q ->
                q
                |> (Option.defaultValue QuestionState.initial)
                |> Some) }
    
//Workflow Running
let runQuizWorklfowEngine getQuiz saveQuiz pureWorkflow mapDbError (command: WithinQuizCommand<'b>) =
    asyncResult {
        let! quiz =
            getQuiz command.Quiz
            |> AsyncResult.mapError mapDbError

        let! quiz, events = pureWorkflow quiz command.Data

        do!
            quiz
            |> Running
            |> saveQuiz
            |> AsyncResult.mapError mapDbError

        return events
    }