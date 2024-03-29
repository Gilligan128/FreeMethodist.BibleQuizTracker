﻿module FreeMethodist.BibleQuizTracker.Server.Tests.CompleteQuiz_UpdateQuizToCompleteTests



open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Tournament
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.CompleteQuiz.Pipeline

[<Fact>]
let ``When completing a quiz Then quiz is in a Completed state`` () =
    let initialQuiz =
        RunningQuiz.newTeamQuiz
        |> fun quiz -> { quiz with Questions = Map.empty }

    let actualQuiz =
        updateQuizToComplete initialQuiz

    let teamOne =
        initialQuiz
        |> RunningQuiz.getTeam TeamPosition.TeamOne

    let teamTwo =
        initialQuiz
        |> RunningQuiz.getTeam TeamPosition.TeamTwo

    let expectedQuiz: CompletedQuiz =
        { Code = initialQuiz.Code
          TournamentInfo = TournamentInfo.empty
          CompetitionStyle =
            CompletedCompetitionStyle.Team(
                { Name = teamOne.Name
                  Score = teamOne.Score
                  Quizzers =
                    teamOne.Quizzers
                    |> List.map (fun q -> { Name = q.Name; Score = q.Score }) },
                { Name = teamTwo.Name
                  Score = teamTwo.Score
                  Quizzers =
                    teamTwo.Quizzers
                    |> List.map (fun q -> { Name = q.Name; Score = q.Score }) }
            )
          CompletedQuestions = [] }

    Assert.Equal(Quiz.Completed expectedQuiz, Quiz.Completed actualQuiz)

let private numberOf value =
    PositiveNumber.create "number" value
    |> function
        | Ok number -> number
        | Error _ -> PositiveNumber.one

[<Fact>]
let ``Given existing questions When completing a quiz Then all questions are complete`` () =

    let unansweredIncorrectAnswerers =
        [ "Juni" ]

    let unansweredQuestion: QuestionState =
        { Prejumps = []
          FailedAppeals = []
          AnswerState =
            QuizAnswer.Complete
            <| Unanswered unansweredIncorrectAnswerers }

    let answeredAnswerState =
        { IncorrectAnswerers = [ "Jim" ]
          Answerer = "Jim" }

    let answeredQuestion: QuestionState =
        { Prejumps = []
          FailedAppeals = ["Jina"]
          AnswerState =
            Answered answeredAnswerState
            |> QuizAnswer.Complete }

    let incompleteAnswerers = [ "Juni" ]

    let incompleteQuestion: QuestionState =
        { Prejumps = []
          FailedAppeals = []
          AnswerState = QuizAnswer.Incomplete <| incompleteAnswerers }

    let initialQuiz =
        RunningQuiz.newTeamQuiz
        |> fun quiz ->
            { quiz with
                Questions =
                    Map.empty
                    |> Map.add PositiveNumber.one unansweredQuestion
                    |> Map.add (numberOf 2) answeredQuestion
                    |> Map.add (numberOf 3) (incompleteQuestion) }


    let actualQuiz =
        updateQuizToComplete initialQuiz

    let actualQuestions =
        actualQuiz.CompletedQuestions

    let expectedQuestions =
        [ {
            Prejumps = []
            FailedAppeals = []
            AnswerState = Unanswered unansweredIncorrectAnswerers }
          {
            Prejumps = []
            FailedAppeals = ["Jina"]
            AnswerState = Answered answeredAnswerState }
          {
            Prejumps = []
            FailedAppeals = []
            AnswerState = Unanswered incompleteAnswerers } ]

    Assert.Equal(expectedQuestions.Length, actualQuestions.Length)
    Assert.Equal<CompletedQuestion list>(expectedQuestions, actualQuestions)
