module FreeMethodist.BibleQuizTracker.Server.ScoreEvents

open System
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open System.Linq

type ScoreEvent =
    | TeamScoreChanged of TeamScoreChanged
    | IndividualScoreChanged of IndividualScoreChanged

let join (innerSequence: 'b seq) outerKeySelector innerKeySelector (outerSequence: 'a seq) =
    outerSequence.Join(
        innerSequence,
        Func<_, _>(outerKeySelector),
        Func<_, _>(innerKeySelector),
        fun outer inner -> (outer, inner)
    )

let private createQuizzerScoreEvents
    quizCode
    currentQuestion
    (oldQuizzers: QuizzerState list, newQuizzers: QuizzerState list)
    =
    oldQuizzers
    |> join newQuizzers (fun o -> o.Name) (fun i -> i.Name)
    |> Seq.map (fun (_, quizzer) ->
        ScoreEvent.IndividualScoreChanged
            { Quiz = quizCode
              Quizzer = quizzer.Name
              Question = currentQuestion
              NewScore = quizzer.Score })

let createScoreEvents (oldQuizState: RunningQuiz) (newQuizState: RunningQuiz) =
    match oldQuizState.CompetitionStyle, newQuizState.CompetitionStyle with
    | RunningCompetitionStyle.Team (oldTeamOne, oldTeamTwo), RunningCompetitionStyle.Team (newTeamOne, newTeamTwo) ->
        let teamEvents =
            [ (oldTeamOne, newTeamOne, TeamOne)
              (newTeamOne, newTeamTwo, TeamTwo) ]
            |> List.choose (fun (oldteam, newTeam, position) ->
                if oldteam.Score <> newTeam.Score then
                    Some(
                        ScoreEvent.TeamScoreChanged
                            { Quiz = newQuizState.Code
                              Team = position
                              NewScore = newTeam.Score }
                    )
                else
                    None)

        let individualEvents =
            [ (oldTeamOne.Quizzers, newTeamOne.Quizzers)
              (oldTeamTwo.Quizzers, newTeamTwo.Quizzers) ]
            |> Seq.collect (createQuizzerScoreEvents newQuizState.Code newQuizState.CurrentQuestion)
            |> Seq.toList

        teamEvents @ individualEvents

    | RunningCompetitionStyle.Individuals oldQuizzers, RunningCompetitionStyle.Individuals newQuizzers ->
        (oldQuizzers, newQuizzers)
        |> createQuizzerScoreEvents newQuizState.Code newQuizState.CurrentQuestion
        |> Seq.toList
    | _, _ -> [] //a quiz should not change its competition style while running.