module FreeMethodist.BibleQuizTracker.Server.SelectQuizzer_Workflow

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

module SelectQuizzer =
    type Input = { Quizzer: Quizzer }
    type Command = Input WithinQuizCommand
    type Error =
        | QuizState of QuizStateError
        | QuizzerNotParticipating of Quizzer
        | QuizzerAlreadyCurrent
        | DbError of DbError
    type Workflow = Command -> AsyncResult<CurrentQuizzerChanged, Error>     