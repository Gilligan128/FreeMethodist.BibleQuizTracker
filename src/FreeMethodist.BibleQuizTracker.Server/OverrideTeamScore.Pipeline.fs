module FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Pipeline

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Pipeline
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

let overrideTeamScoreAsync getQuiz (saveQuiz:SaveTeamQuizAsync) : OverrideTeamScore.Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.ofAsync
            let! quiz = validateQuiz quiz |> AsyncResult.ofResult
            let score = command.Data
            let quiz = updateQuizScore quiz score
            let event = createEvent quiz score
            do! saveQuiz (Running quiz) |> AsyncResult.ofAsync
            return event
        }
        