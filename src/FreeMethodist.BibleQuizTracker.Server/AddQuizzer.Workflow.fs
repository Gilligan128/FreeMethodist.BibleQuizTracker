module FreeMethodist.BibleQuizTracker.Server.AddQuizzer_Workflow

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow


[<RequireQualifiedAccess>]
module AddQuizzer =
    type Data = { Name: Quizzer; Team: TeamPosition }
    type Command = WithinQuizCommand<Data>

    type Error =
        | QuizState of QuizStateError
        | QuizzerAlreadyAdded of Quizzer
        | DbError of DbError

    type Workflow = Command -> AsyncResult<QuizzerParticipating, Error>
