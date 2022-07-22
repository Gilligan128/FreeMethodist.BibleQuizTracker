module FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Workflow

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

module ClearAppeal =
    type Command = WithinQuizCommand<unit>
    
    type Error =
        | QuizState of QuizStateError
        | NoFailedAppeal
    
    type Event = TeamScoreChanged of TeamScoreChanged
    
    type Workflow = Command -> AsyncResult<Event list, Error>