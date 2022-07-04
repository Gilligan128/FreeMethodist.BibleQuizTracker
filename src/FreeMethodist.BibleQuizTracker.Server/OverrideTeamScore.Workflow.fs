module FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Api

open System
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi

type TeamPosition =
    | TeamOne
    | TeamTwo
    
type OverrideTeamScoreData = {
    Team: TeamPosition
    NewScore: TeamScore
}

type TeamScoreChanged = {
    Quiz: QuizCode
    Team: TeamPosition
    NewScore: TeamScore
}

[<RequireQualifiedAccess>]
module OverrideTeamScore =
    type Command = WithinQuizCommand<OverrideTeamScoreData>
    
    type Error =
        | QuizNotFound of QuizCode
        | TeamNotInQuiz of TeamName
        | WrongQuizState of Type
   
    type Workflow = Command -> Result<TeamScoreChanged, Error>

