module FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
 
[<RequireQualifiedAccess>]
module MoveQuestion = 

    type QuestionData = { Question:QuestionNumber }
    type Command = WithinQuizCommand<QuestionData>
    type Workflow = Command -> Result<CurrentQuestionChanged, QuizStateError>
    