﻿module FreeMethodist.BibleQuizTracker.Server.CompleteQuiz.Pipeline

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Collections

let private mapRunningQuizzerToComplete (quizzer: QuizzerState) : CompletedQuizzer =
    { Name = quizzer.Name
      Score = quizzer.Score }

let private mapRunningTeamToComplete (team: QuizTeamState) : CompletedTeam =
    { Name = team.Name
      Score = team.Score
      Quizzers =
        team.Quizzers
        |> List.map mapRunningQuizzerToComplete }

let private mapRunningQuestionToComplete (question: QuestionState) : CompletedQuestion =
    { Prejumps = question.Prejumps
      FailedAppeals = question.FailedAppeals
      AnswerState =
        match question.AnswerState with
        | Incomplete attempts -> Unanswered attempts
        | QuizAnswer.Complete completed -> completed }

let updateQuizToComplete (quiz: RunningQuiz) : CompletedQuiz =

    { Code = quiz.Code
      TournamentInfo = quiz.TournamentInfo
      CompetitionStyle =
        match quiz.CompetitionStyle with
        | RunningCompetitionStyle.Team(teamOne, teamTwo) -> ((mapRunningTeamToComplete teamOne), (mapRunningTeamToComplete teamTwo)) |> CompletedCompetitionStyle.Team
        | RunningCompetitionStyle.Individuals quizzerStates -> quizzerStates |> List.map mapRunningQuizzerToComplete |> CompletedCompetitionStyle.Individual  
        
      CompletedQuestions =
        quiz.Questions
        |> Map.toList
        |> List.map snd
        |> List.map mapRunningQuestionToComplete }

let completeQuiz getQuiz saveQuiz : CompleteQuiz.Workflow =
    fun command ->
        asyncResult {
            let! quiz = command |> getQuiz |> AsyncResult.mapError CompleteQuiz.DbError

            let! validQuiz =
                quiz
                |> validateRunningQuiz
                |> Result.mapError CompleteQuiz.QuizState
                |> AsyncResult.ofResult

            let updatedQuiz =
                validQuiz |> updateQuizToComplete

            do!
                updatedQuiz
                |> Quiz.Completed
                |> saveQuiz
                |> AsyncResult.mapError CompleteQuiz.DbError

            return [CompleteQuiz.Event.QuizStateChanged {  Quiz = validQuiz.Code; NewState = nameof Completed}]
        }
