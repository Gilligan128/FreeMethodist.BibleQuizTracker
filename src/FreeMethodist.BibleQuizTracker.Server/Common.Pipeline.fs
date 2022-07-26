module FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core
open Elmish
//We will eventually modularize this into separate files, probbaly by Workflow.

//Domain Model


type QuizRoomState =
    | Open
    | Closed


type CompletedQuizzer = { Name: Quizzer; Score: int }

type CompletedTeam =
    { Name: TeamName
      Score: int
      Quizzers: Quizzer list }

type CompletedTeamQuiz =
    { code: QuizCode
      winningTeam: CompletedTeam
      losingTeam: CompletedTeam
      completedQuestions: CompletedQuestion list }

type OfficialQuizzer = { Name: Quizzer; Score: TeamScore }

type OfficialTeam =
    { Name: TeamName
      Score: TeamScore
      QuizzerOne: OfficialQuizzer
      QuizzerTwo: OfficialQuizzer
      QuizzerThree: OfficialQuizzer option
      QuizzerFour: OfficialQuizzer option
      QuizzerFive: OfficialQuizzer option }

type OfficialTeamQuiz =
    { Code: QuizCode
      WinningTeam: OfficialTeam
      LosingTeam: OfficialTeam
      CompletedQuestions: CompletedQuestion list }

type Quiz =
    | Unvalidated of UnvalidatedTeamQuiz
    | Running of RunningTeamQuiz
    | Completed of CompletedTeamQuiz
    | Official of OfficialTeamQuiz

//Persistence
type GetTeamQuizAsync = QuizCode -> AsyncResult<Quiz, DbError>
type SaveTeamQuizAsync = Quiz -> AsyncResult<unit, DbError>


type GetJumps = QuizCode -> Jump seq
type SaveJump = Jump -> unit

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
type ValidateQuizIsRunning = Quiz -> Result<RunningTeamQuiz, QuizStateError>
type ValidateCurrentQuizzer = RunningTeamQuiz -> Result<Quizzer, NoCurrentQuizzer>
type ValidateCurrentQuizzerWithTeam = RunningTeamQuiz -> Result<Quizzer * TeamPosition, NoCurrentQuizzer>

let validateCurrentQuizzer: ValidateCurrentQuizzer =
    fun quiz ->
        quiz.CurrentQuizzer
        |> (Result.ofOption NoCurrentQuizzer)

let validateCurrentQuizzerWithTeam: ValidateCurrentQuizzerWithTeam =
    fun quiz ->
        let quizzerOpt quizzers q =
            quizzers
            |> List.tryFind (QuizzerState.isQuizzer q)
            |> Option.map (fun _ -> q)

        let teamOneOpt =
            quiz.CurrentQuizzer
            |> Option.bind (fun q -> q |> quizzerOpt quiz.TeamOne.Quizzers)

        let teamTwoOpt =
            quiz.CurrentQuizzer
            |> Option.bind (fun q -> q |> quizzerOpt quiz.TeamTwo.Quizzers)

        match teamOneOpt, teamTwoOpt with
        | None, None -> Error NoCurrentQuizzer
        | Some quizzer, None -> Ok (quizzer, TeamOne)
        | None, Some quizzer -> Ok (quizzer, TeamTwo)
        | Some quizzer1, Some _ -> Ok (quizzer1, TeamOne) //should be an error, really



let validateQuiz: ValidateQuizIsRunning =
    fun quiz ->
        match quiz with
        | Quiz.Running running -> Ok running
        | Quiz.Completed c -> Error(WrongQuizState(c.GetType()))
        | Quiz.Official o -> Error(WrongQuizState(o.GetType()))
        | Quiz.Unvalidated u -> Error(WrongQuizState(u.GetType()))

//Score calculation
type CalculateQuizzerScore = Map<QuestionNumber, QuizAnswer> -> Quizzer -> TeamScore
type CalculateTeamScore = Map<Quizzer, TeamScore> -> TeamScore
