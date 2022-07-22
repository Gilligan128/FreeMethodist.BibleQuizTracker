module FreeMethodist.BibleQuizTracker.Server.FailAppeal.Workflow

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

module FailAppeal =
    type Command = WithinQuizCommand<unit>

    type Error =
        | QuizState
        | NoCurrentQuizzer of NoCurrentQuizzer
        | AppealAlreadyFailed of Quizzer

    type Event = TeamScoreChanged of TeamScoreChanged

    type Workflow = Command -> AsyncResult<Event list, Error>
