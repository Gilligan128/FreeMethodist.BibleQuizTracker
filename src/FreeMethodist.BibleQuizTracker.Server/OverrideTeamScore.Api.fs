module FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Api

open FreeMethodist.BibleQuizTracker.Server.QuizzingApi

type UnvalidatedTeamScore = {
    Team: TeamName
    NewScore: TeamScore
}

type TeamScoreChanged = {
    Quiz: QuizCode
    Team: TeamName
    NewScore: TeamScore
}

[<RequireQualifiedAccess>]
module OverrideTeamScore =
    type Command = WithinQuizCommand<UnvalidatedTeamScore>
    
    type Error =
        | QuizNotFound of QuizCode
        | TeamNotInQuiz of TeamName
   
    type Workflow = Command -> Result<TeamScoreChanged, Error>

