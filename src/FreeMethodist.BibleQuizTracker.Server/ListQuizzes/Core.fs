module FreeMethodist.BibleQuizTracker.Server.ListQuizzes.Core

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Tournament

let private mapTournamentLink tournament =
    match tournament with
    | TournamentLink.Id _ -> None
    | TournamentLink.Name code -> Some code

let private mapTournamentInfo tournamentInfo quizItem =
    { quizItem with
        Tournament = tournamentInfo.Link |> Option.bind mapTournamentLink
        Room = tournamentInfo.Room
        Round = tournamentInfo.Round }

let private initialQuizItem =
    { Code = ""
      Tournament = None
      State = ListQuizState.Running
      Room = None
      Round = None }

let mapQuizToListQuizItem (quiz: Quiz) : ListQuizItem =
    match quiz with
    | Quiz.Running quiz ->
        {initialQuizItem with  Code = quiz.Code; State = ListQuizState.Running }
        |> mapTournamentInfo quiz.TournamentInfo
    | Quiz.Completed quiz ->
        {initialQuizItem with  Code = quiz.Code; State = ListQuizState.Completed }
        |> mapTournamentInfo quiz.TournamentInfo
    | Quiz.Official quiz ->
        {initialQuizItem with  Code = quiz.Code; State = ListQuizState.Official }
        |> mapTournamentInfo quiz.TournamentInfo
