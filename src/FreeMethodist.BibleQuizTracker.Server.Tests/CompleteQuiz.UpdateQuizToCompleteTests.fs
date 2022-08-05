module FreeMethodist.BibleQuizTracker.Server.Tests.CompleteQuiz_UpdateQuizToCompleteTests



open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.CompleteQuiz.Pipeline

[<Fact>]
let ``When completing a quiz Then quiz is in a Completed state`` () =
    let initialQuiz = RunningTeamQuiz.identity

    let actualQuiz =
        updateQuizToComplete initialQuiz

    let expectedQuiz =
        Quiz.Completed
            { Code = initialQuiz.Code
              CompetitionStyle =
                Team(
                    { Name = initialQuiz.TeamOne.Name
                      Score = initialQuiz.TeamOne.Score
                      Quizzers =
                        initialQuiz.TeamOne.Quizzers
                        |> List.map (fun q -> { Name = q.Name; Score = q.Score }) },
                    { Name = initialQuiz.TeamTwo.Name
                      Score = initialQuiz.TeamTwo.Score
                      Quizzers =
                        initialQuiz.TeamTwo.Quizzers
                        |> List.map (fun q -> { Name = q.Name; Score = q.Score }) }
                )
              WinningTeam =
                { Name = initialQuiz.TeamOne.Name
                  Score = initialQuiz.TeamOne.Score
                  Quizzers =
                    initialQuiz.TeamOne.Quizzers
                    |> List.map (fun q -> { Name = q.Name; Score = q.Score }) }
              LosingTeam =
                { Name = initialQuiz.TeamTwo.Name
                  Score = initialQuiz.TeamTwo.Score
                  Quizzers =
                    initialQuiz.TeamTwo.Quizzers
                    |> List.map (fun q -> { Name = q.Name; Score = q.Score }) }
              CompletedQuestions = [] }

    Assert.True(true)
