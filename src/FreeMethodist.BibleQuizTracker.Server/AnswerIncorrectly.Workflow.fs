module FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

module AnswerIncorrectly =
    type Command = WithinQuizCommand<unit>
    type Event =
        | CurrentQuizzerChanged of CurrentQuizzerChanged
    type Error =
        | QuizState of QuizStateError
    type Workflow = Command