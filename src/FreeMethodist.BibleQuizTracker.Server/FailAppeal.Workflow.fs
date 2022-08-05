﻿module FreeMethodist.BibleQuizTracker.Server.FailAppeal.Workflow

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core

[<RequireQualifiedAccess>]
module FailAppeal =
    type Command = WithinQuizCommand<unit>

    type Error =
        | QuizState of QuizStateError
        | NoCurrentQuizzer of NoCurrentQuizzer
        | AppealAlreadyFailed of Quizzer
        | DbError of DbError

    type Event = TeamScoreChanged of TeamScoreChanged

    type Workflow = Command -> AsyncResult<Event list, Error>
