module FreeMethodist.BibleQuizTracker.Server.RemoveQuizzer_Workflow

open FreeMethodist.BibleQuizTracker.Server.AddQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core



[<RequireQualifiedAccess>]
module RemoveQuizzer =
    type Data = { Quizzer: Quizzer; Team: TeamPosition }
    type Command = WithinQuizCommand<Data>
    type Error =
        | QuizStateError of QuizStateError
        | QuizzerNotParticipating of Quizzer
        | DbError of DbError
    type Event =
        | CurrentQuizzerChanged of CurrentQuizzerChanged
        | QuizzerNoLongerParticipating of QuizzerNoLongerParticipating
    type Workflow = Command -> AsyncResult<Event list,Error>
    
    