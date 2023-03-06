module FreeMethodist.BibleQuizTracker.Server.ListQuizzes.Core

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Tournament
open FreeMethodist.BibleQuizTracker.Server.Workflow

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
      CompetitionStyle = ListCompetitionStyle.Individual 0
      Room = None
      Round = None }

let mapQuizToListQuizItem (quiz: Quiz) : ListQuizItem =
    match quiz with
    | Quiz.Running quiz ->
        { initialQuizItem with
            Code = quiz.Code
            State = ListQuizState.Running
            CompetitionStyle =
                match quiz.CompetitionStyle with
                | RunningCompetitionStyle.Individuals people -> ListCompetitionStyle.Individual people.Length
                | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
                    ListCompetitionStyle.Team(teamOne.Name, teamTwo.Name) }
        |> mapTournamentInfo quiz.TournamentInfo
    | Quiz.Completed quiz ->
        { initialQuizItem with
            Code = quiz.Code
            State = ListQuizState.Completed
            CompetitionStyle =
                match quiz.CompetitionStyle with
                | CompletedCompetitionStyle.Individual people -> ListCompetitionStyle.Individual people.Length
                | CompletedCompetitionStyle.Team (teamOne, teamTwo) ->
                    ListCompetitionStyle.Team(teamOne.Name, teamTwo.Name) }
        |> mapTournamentInfo quiz.TournamentInfo
    | Quiz.Official quiz ->
        { initialQuizItem with
            Code = quiz.Code
            State = ListQuizState.Official
            CompetitionStyle =
                match quiz.CompetitionStyle with
                | OfficialCompetitionStyle.Individual people -> ListCompetitionStyle.Individual people.Length
                | OfficialCompetitionStyle.Team (teamOne, teamTwo) ->
                    ListCompetitionStyle.Team(teamOne.Name, teamTwo.Name) }
        |> mapTournamentInfo quiz.TournamentInfo
