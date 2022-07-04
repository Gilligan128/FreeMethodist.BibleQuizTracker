module FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Pipeline

open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Api
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain
open Microsoft.FSharp.Core


type ValidateQuiz = GetTeamQuiz -> QuizCode -> Result<RunningTeamQuiz, OverrideTeamScore.Error>
type ValidateTeamScore = RunningTeamQuiz -> OverrideTeamScoreData -> Result<TeamScore, OverrideTeamScore.Error>
type UpdateQuizScore = RunningTeamQuiz -> OverrideTeamScoreData -> RunningTeamQuiz
type CreateEvent = RunningTeamQuiz -> OverrideTeamScoreData -> TeamScoreChanged

let validateQuiz: ValidateQuiz =
    fun getQuiz code ->
        let quizResult = getQuiz code

        match quizResult with
        | TeamQuiz.Running running -> Ok running
        | TeamQuiz.Completed c -> Error(OverrideTeamScore.Error.WrongQuizState(c.GetType()))
        | TeamQuiz.Official o -> Error(OverrideTeamScore.Error.WrongQuizState(o.GetType()))
        | TeamQuiz.Unvalidated u -> Error(OverrideTeamScore.Error.WrongQuizState(u.GetType()))

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

let overrideTeamScore getQuiz saveQuiz : OverrideTeamScore.Workflow =
    fun command ->
        result {
            let! quiz = validateQuiz getQuiz command.Quiz
            let score = command.Data
            let quiz = updateQuizScore quiz score
            let event = createEvent quiz score
            saveQuiz  quiz
            return event
        }
    