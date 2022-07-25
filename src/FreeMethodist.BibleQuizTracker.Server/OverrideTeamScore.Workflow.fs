module FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Workflow

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow


type OverrideTeamScoreData =
    { Team: TeamPosition
      NewScore: TeamScore }


[<RequireQualifiedAccess>]
module OverrideTeamScore =
    type Command = WithinQuizCommand<OverrideTeamScoreData>

    type Error = | QuizState of QuizStateError
                 | DbError of DbError
    
    type Workflow = Command -> AsyncResult<TeamScoreChanged, Error>
