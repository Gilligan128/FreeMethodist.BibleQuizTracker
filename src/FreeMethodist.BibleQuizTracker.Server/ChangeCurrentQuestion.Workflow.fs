﻿module FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

[<RequireQualifiedAccess>]
module ChangeCurrentQuestion =

    type QuestionData = { Question: QuestionNumber }
    type Command = WithinQuizCommand<QuestionData>
    
    type Error = | QuizState of QuizStateError
                 | DbError of DbError
    type Workflow = Command -> AsyncResult<CurrentQuestionChanged, Error>
