module FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Pipeline

open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Api
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain
open Microsoft.FSharp.Core

type TeamBeingUpdated =
    | TeamOne
    | TeamTwo

type ValidTeamScore =
    { Score: TeamScore
      TeamBeingUpdated: TeamBeingUpdated }

type ValidateQuiz = GetTeamQuiz -> QuizCode -> Result<RunningTeamQuiz, OverrideTeamScore.Error>
type ValidateTeamScore = RunningTeamQuiz -> UnvalidatedTeamScore -> Result<ValidTeamScore, OverrideTeamScore.Error>
type UpdateQuizScore = RunningTeamQuiz -> ValidTeamScore -> RunningTeamQuiz
type CreateEvent = RunningTeamQuiz -> ValidTeamScore -> TeamScoreChanged

let validateQuiz: ValidateQuiz =
    fun getQuiz code ->
        let quizResult = getQuiz code

        match quizResult with
        | TeamQuiz.Running running -> Ok running
        | TeamQuiz.Completed c -> Error(OverrideTeamScore.Error.WrongQuizState(c.GetType()))
        | TeamQuiz.Official o -> Error(OverrideTeamScore.Error.WrongQuizState(o.GetType()))
        | TeamQuiz.Unvalidated u -> Error(OverrideTeamScore.Error.WrongQuizState(u.GetType()))

let validateTeamScore: ValidateTeamScore =
    fun teamQuiz unvalidatedScore ->
        if (teamQuiz.TeamOne.Name = unvalidatedScore.Team) then
            Ok
                { Score = unvalidatedScore.NewScore
                  TeamBeingUpdated = TeamOne }
        else
            Result.Error(OverrideTeamScore.Error.TeamNotInQuiz unvalidatedScore.Team)

let updateQuizScore: UpdateQuizScore =
    fun quiz score ->
        match score.TeamBeingUpdated with
        | TeamOne -> { quiz with TeamOne = { quiz.TeamOne with Score = score.Score } }
        | TeamTwo -> { quiz with TeamTwo = { quiz.TeamTwo with Score = score.Score } }

let createEvent: CreateEvent =
    fun quiz score ->
        { Team =
            match score.TeamBeingUpdated with
            | TeamOne -> quiz.TeamOne.Name
            | TeamTwo -> quiz.TeamTwo.Name
          NewScore = score.Score
          Quiz = quiz.Code }

let overrideTeamScore getQuiz saveQuiz : OverrideTeamScore.Workflow =
    fun command ->
        result {
            let! quiz = validateQuiz getQuiz command.Quiz
            let! score = validateTeamScore quiz command.Data
            let quiz = updateQuizScore quiz score
            let event = createEvent quiz score
            saveQuiz quiz
            return event
        }
    