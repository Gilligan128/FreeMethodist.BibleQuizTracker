namespace global

open FreeMethodist.BibleQuizTracker.Server.Workflow

[<RequireQualifiedAccess>]
module Arrange =
    let withParticipants quizzers (quiz: RunningQuiz) =
        match quiz.CompetitionStyle with
        | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
            let teamOne =
                { teamOne with Quizzers = quizzers }

            { quiz with CompetitionStyle = RunningCompetitionStyle.Team(teamOne, teamTwo) }
        | RunningCompetitionStyle.Individuals _ ->
            { quiz with CompetitionStyle = RunningCompetitionStyle.Individuals quizzers }

    let withTeamTwoParticipants quizzers (quiz: RunningQuiz) =
        match quiz.CompetitionStyle with
        | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
            let teamTwo =
                { teamTwo with Quizzers = quizzers }

            { quiz with CompetitionStyle = RunningCompetitionStyle.Team(teamOne, teamTwo) }
        | RunningCompetitionStyle.Individuals _ ->
            { quiz with CompetitionStyle = RunningCompetitionStyle.Individuals quizzers }
