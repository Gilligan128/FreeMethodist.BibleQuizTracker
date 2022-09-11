﻿module FreeMethodist.BibleQuizTracker.Server.Tests.RemoveQuizzerWorkflowTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit

let initialTeamStateWithQuizzer quizzer =
    { RunningTeamQuiz.identity.TeamOne with
        Quizzers =
            RunningTeamQuiz.identity.TeamOne.Quizzers
            @ [ { Name = quizzer
                  Participation = In
                  Score = TeamScore.zero } ] }



[<Fact>]
let ``Given there is no jump order, when current quizzer removed then there is no current quizzer`` () =
    let input: RemoveQuizzer.Data =
        { Quizzer = "Juni"}

    let initialQuizState =
        { RunningTeamQuiz.identity with
            CurrentQuizzer = Some input.Quizzer
            CompetitionStyle = (initialTeamStateWithQuizzer input.Quizzer, { Name = ""; Score = TeamScore.zero; Quizzers = [] }) |> RunningCompetitionStyle.Team
            TeamOne = initialTeamStateWithQuizzer input.Quizzer }

    let quiz, currentChangedEvent =
        RemoveQuizzer_Pipeline.removeQuizzerFromQuiz input initialQuizState []

    Assert.NotEqual(Some input.Quizzer, quiz.CurrentQuizzer)
    Assert.Equal(None, quiz.CurrentQuizzer)
    Assert.NotEqual(None, currentChangedEvent)

    Assert.Equal(
        None,
        currentChangedEvent
        |> Option.bind (fun event -> event.CurrentQuizzer)
    )

[<Fact>]
let ``when a non-current quizzer removed then Current Quizzer remains the same`` () =
    let input: RemoveQuizzer.Data =
        { Quizzer = "Juni" }

    let initialQuizState =
        { RunningTeamQuiz.identity with
            CurrentQuizzer = Some $"Not {input.Quizzer}"
            CompetitionStyle = (initialTeamStateWithQuizzer input.Quizzer, { Name = ""; Score = TeamScore.zero; Quizzers = [] }) |> RunningCompetitionStyle.Team
            TeamOne = initialTeamStateWithQuizzer input.Quizzer }

    let quiz, currentChangedEvent =
        RemoveQuizzer_Pipeline.removeQuizzerFromQuiz input initialQuizState []

    Assert.Equal(initialQuizState.CurrentQuizzer, quiz.CurrentQuizzer)
    Assert.Equal(None, currentChangedEvent)

