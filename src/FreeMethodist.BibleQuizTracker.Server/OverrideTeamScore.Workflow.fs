﻿module FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Workflow

open System
open FreeMethodist.BibleQuizTracker.Server.Workflow


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
    
    type Workflow = Command -> Result<TeamScoreChanged, QuizStateError>

