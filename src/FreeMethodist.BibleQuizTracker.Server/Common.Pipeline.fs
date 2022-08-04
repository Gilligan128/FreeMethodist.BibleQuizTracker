module FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core
open Elmish

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
    | Running of RunningTeamQuiz
    | Completed of CompletedTeamQuiz
    | Official of OfficialTeamQuiz

[<RequireQualifiedAccess>]
module Quiz =
    let start unvalidatedQuiz =
        match unvalidatedQuiz.CompetitionStyle with
        | Individual -> Result.Error ()
        | Team teams ->
            Running
                { Code = unvalidatedQuiz.Code
                  TeamOne =
                    { Name = teams.TeamOneName
                      Score = TeamScore.initial
                      Quizzers = []
                   }
                  TeamTwo =
                    { Name = teams.TeamTwoName
                      Score = TeamScore.initial
                      Quizzers = [] }
                  CurrentQuizzer = None
                  CurrentQuestion = PositiveNumber.one
                  Questions =
                    Map.empty
                    |> Map.add PositiveNumber.one QuestionState.initial}
            |> Ok

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



let validateQuiz: ValidateQuizIsRunning =
    fun quiz ->
        match quiz with
        | Quiz.Running running -> Ok running
        | Quiz.Completed c -> Error(WrongQuizState(c.GetType()))
        | Quiz.Official o -> Error(WrongQuizState(o.GetType()))

//Score calculation

//type QuestionQuizzerEvents = 

//Changing current question
let changeCurrentQuestionInQuiz question quiz  =
    { quiz with
        CurrentQuestion = question
        Questions =
            quiz.Questions
            |> Map.change quiz.CurrentQuestion (fun q ->
                q
                |> Option.defaultValue (QuestionState.create ([] |> Unanswered |> Complete))
                |> Some)
            |> Map.change question (fun q -> q |> (Option.defaultValue QuestionState.initial) |> Some)  }
