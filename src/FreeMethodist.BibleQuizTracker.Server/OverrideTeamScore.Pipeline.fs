module FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Pipeline

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core



type UpdateQuizScore = RunningTeamQuiz -> OverrideTeamScoreData -> RunningTeamQuiz
type CreateEvent = RunningTeamQuiz -> OverrideTeamScoreData -> TeamScoreChanged


let updateQuizScore: UpdateQuizScore =
    fun quiz score ->
        match score.Team with
        | TeamOne -> { quiz with TeamOne = { quiz.TeamOne with Score = score.NewScore } }
        | TeamTwo -> { quiz with TeamTwo = { quiz.TeamTwo with Score = score.NewScore } }

let createEvent: CreateEvent =
    fun quiz score ->
        { Team = score.Team
          NewScore = score.NewScore
          Quiz = quiz.Code }

let overrideTeamScoreAsync getQuiz (saveQuiz: SaveTeamQuiz) : OverrideTeamScore.Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.mapError OverrideTeamScore.DbError

            let! quiz =
                validateQuiz quiz
                |> AsyncResult.ofResult
                |> AsyncResult.mapError OverrideTeamScore.QuizState

            let score = command.Data
            let quiz = updateQuizScore quiz score
            let event = createEvent quiz score

            do!
                saveQuiz (Running quiz)
                |> AsyncResult.mapError OverrideTeamScore.DbError

            return event
        }
