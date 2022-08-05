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
 
 [<RequireQualifiedAccess>]
 module SelectQuizzer =
    type Input = { Quizzer: Quizzer }
    type Command = Input WithinQuizCommand
    type Error =
        | QuizState of QuizStateError
        | QuizzerNotParticipating of Quizzer
        | QuizzerAlreadyCurrent
        | DbError of DbError
    type Workflow = Command -> AsyncResult<CurrentQuizzerChanged, Error>     