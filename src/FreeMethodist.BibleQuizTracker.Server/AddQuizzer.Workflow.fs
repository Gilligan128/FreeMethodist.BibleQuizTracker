module FreeMethodist.BibleQuizTracker.Server.AddQuizzer_Workflow

open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

type QuizzerParticipating = { Quizzer: Quizzer; Quiz: QuizCode }

[<RequireQualifiedAccess>]
module AddQuizzer =
    type Data = { Name: Quizzer; Team: TeamPosition }
    type Command = WithinQuizCommand<Data>
    type Error =
        | QuizState of QuizStateError
        | QuizzerAlreadyAdded of Quizzer
    type Workflow = Command -> Result<QuizzerParticipating, Error>