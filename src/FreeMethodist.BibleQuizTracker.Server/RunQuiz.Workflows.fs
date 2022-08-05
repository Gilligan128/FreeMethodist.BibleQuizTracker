module FreeMethodist.BibleQuizTracker.Server.Workflows

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

[<RequireQualifiedAccess>]
module ChangeCurrentQuestion =

    type QuestionData = { Question: QuestionNumber }
    type Command = WithinQuizCommand<QuestionData>
    
    type Error = | QuizState of QuizStateError
                 | DbError of DbError
    type Workflow = Command -> AsyncResult<CurrentQuestionChanged, Error>

[<RequireQualifiedAccess>]
module AddQuizzer =
    type Data = { Name: Quizzer; Team: TeamPosition }
    type Command = WithinQuizCommand<Data>

    type Error =
        | QuizState of QuizStateError
        | QuizzerAlreadyAdded of Quizzer
        | DbError of DbError

    type Workflow = Command -> AsyncResult<QuizzerParticipating, Error>
