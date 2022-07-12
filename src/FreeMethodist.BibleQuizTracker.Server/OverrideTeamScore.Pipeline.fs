module FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Pipeline

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

let overrideTeamScore getQuiz (saveQuiz:SaveTeamQuiz) : OverrideTeamScore.Workflow =
    fun command ->
        result {
            let quiz = getQuiz command.Quiz
            let! quiz = validateQuiz quiz
            let score = command.Data
            let quiz = updateQuizScore quiz score
            let event = createEvent quiz score
            saveQuiz (Running quiz)
            return event
        }
    