﻿module FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow

open FreeMethodist.BibleQuizTracker.Server.QuizzingApi

type QuestionChanged = { Quiz: QuizCode; NewQuestion: QuestionNumber; }
 
[<RequireQualifiedAccess>]
module MoveQuestion = 

    type QuestionData = { Question:QuestionNumber }
    type Command = WithinQuizCommand<QuestionData>
    type Workflow = Command -> Result<QuestionChanged, QuizStateError>
    