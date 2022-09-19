module FreeMethodist.BibleQuizTracker.Server.Tests.RemoveQuizzerWorkflowTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit


[<Fact>]
let ``Given there is no jump order, when current quizzer removed then there is no current quizzer`` () =
    let input: RemoveQuizzer.Data =
        { Quizzer = "Juni" }

    let initialQuizState =
        { RunningQuiz.newTeamQuiz with
            CurrentQuizzer = Some input.Quizzer }
        |> Arrange.withParticipants [QuizzerState.create input.Quizzer ]

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
        { RunningQuiz.newTeamQuiz with
            CurrentQuizzer = Some $"Not {input.Quizzer}"
            }
        |> Arrange.withParticipants [QuizzerState.create input.Quizzer]

    let quiz, currentChangedEvent =
        RemoveQuizzer_Pipeline.removeQuizzerFromQuiz input initialQuizState []

    Assert.Equal(initialQuizState.CurrentQuizzer, quiz.CurrentQuizzer)
    Assert.Equal(None, currentChangedEvent)

[<Fact>]
let ``when removing a quizzer in Individuals then Quizzer is not in roster`` () =
    let input: RemoveQuizzer.Data =
        { Quizzer = "Juni" }

    let initialQuizState =
        { RunningQuiz.newTeamQuiz with
            CurrentQuizzer = Some $"Not {input.Quizzer}"
            CompetitionStyle =
                [ QuizzerState.create input.Quizzer ]
                |> RunningCompetitionStyle.Individuals }

    let quiz, _ =
        RemoveQuizzer_Pipeline.removeQuizzerFromQuiz input initialQuizState []

    let roster =
        match quiz.CompetitionStyle with
        | RunningCompetitionStyle.Team _ -> None
        | RunningCompetitionStyle.Individuals quizzers -> quizzers |>  List.map (fun q -> q.Name) |> Some
        
    Assert.Equal(Some [], roster)
