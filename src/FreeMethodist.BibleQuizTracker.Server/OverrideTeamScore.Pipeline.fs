module FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Pipeline

open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Api
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain


type ValidatedTeamScore =
    { Quiz: QuizCode
      Team: TeamName
      Score: TeamScore }

type ValidateTeamScore =
    GetQuiz -> QuizCode -> UnvalidatedTeamScore -> Result<ValidatedTeamScore, OverrideTeamScore.Error>

type CreateEvent = ValidateTeamScore -> TeamScoreChanged

let validateTeamScore: ValidateTeamScore =
    fun getQuiz code unvalidatedScore ->
        let quizResult = getQuiz code

        let validateTeamQuiz (teamQuiz: RunningTeamQuiz) =
            if (teamQuiz.TeamOne.Name = unvalidatedScore.Team
               || teamQuiz.TeamTwo.Name = unvalidatedScore.Team) then
                Ok
                    { Quiz = code
                      Team = unvalidatedScore.Team
                      Score = unvalidatedScore.NewScore }
            else
                Result.Error (OverrideTeamScore.Error.TeamNotInQuiz unvalidatedScore.Team)

        match quizResult with
        | TeamQuiz teamQuiz ->
            match teamQuiz with
            | TeamQuiz.Running running ->  running |> validateTeamQuiz
