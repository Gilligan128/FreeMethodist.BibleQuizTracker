module FreeMethodist.BibleQuizTracker.Server.RemoveQuizzer_Workflow

open FreeMethodist.BibleQuizTracker.Server.AddQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core

type QuizzerNoLongerParticipating = { Quizzer : Quizzer; Quiz: QuizCode }

[<RequireQualifiedAccess>]
module RemoveQuizzer =
    type Data = { Quizzer: Quizzer; Team: TeamPosition }
    type Command = WithinQuizCommand<Data>
    type Error =
        | QuizStateError of QuizStateError
        | QuizzerNotParticipating of Quizzer
    type Workflow = Command -> Result<QuizzerNoLongerParticipating, Error>
    
    