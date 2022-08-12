module FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core
open Elmish

//Domain Model

type QuizRoomState =
    | Open
    | Closed


type CompletedQuizzer = { Name: Quizzer; Score: TeamScore }

type CompletedTeam =
    { Name: TeamName
      Score: TeamScore
      Quizzers: CompletedQuizzer list }

type CompletedCompetitionStyle =
    | Individual of CompletedQuizzer list
    | Team of CompletedTeam * CompletedTeam

type CompletedQuestion =
    { FailedAppeal: Quizzer option
      AnswerState: CompletedAnswer }

type CompletedQuiz =
    { Code: QuizCode
      CompetitionStyle: CompletedCompetitionStyle
      CompletedQuestions: CompletedQuestion list }

type OfficialTeam =
    { Name: TeamName
      Score: TeamScore
      QuizzerOne: CompletedQuizzer
      QuizzerTwo: CompletedQuizzer
      QuizzerThree: CompletedQuizzer option
      QuizzerFour: CompletedQuizzer option
      QuizzerFive: CompletedQuizzer option }

[<RequireQualifiedAccess>]
module OfficialTeam =
    let quizzerList (team : OfficialTeam) =
         [ Some team.QuizzerOne; Some team.QuizzerTwo; team.QuizzerThree; team.QuizzerFour; team.QuizzerFive]

type OfficialCompetitionStyle =
    | Individual of CompletedQuizzer list
    | Team of OfficialTeam*OfficialTeam

type OfficialTeamQuiz =
    { Code: QuizCode
      WinningTeam: OfficialTeam
      LosingTeam: OfficialTeam
      CompetitionStyle : OfficialCompetitionStyle
      CompletedQuestions: CompletedQuestion list }

type Quiz =
    | Running of RunningTeamQuiz
    | Completed of CompletedQuiz
    | Official of OfficialTeamQuiz

[<RequireQualifiedAccess>]
module Quiz =
    let start (unvalidatedQuiz: UnvalidatedQuiz) =
        match unvalidatedQuiz.CompetitionStyle with
        | CompetitionStyle.Individual -> Result.Error()
        | CompetitionStyle.Team teams ->
            Running
                { Code = unvalidatedQuiz.Code
                  TeamOne =
                    { Name = teams.TeamOneName
                      Score = TeamScore.initial
                      Quizzers = [] }
                  TeamTwo =
                    { Name = teams.TeamTwoName
                      Score = TeamScore.initial
                      Quizzers = [] }
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
        | Some quizzer, None -> Ok(quizzer, TeamOne)
        | None, Some quizzer -> Ok(quizzer, TeamTwo)
        | Some quizzer1, Some _ -> Ok(quizzer1, TeamOne) //should be an error, really

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

//Score calculation

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
