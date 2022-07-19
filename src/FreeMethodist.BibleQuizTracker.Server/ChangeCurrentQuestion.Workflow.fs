module FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

[<RequireQualifiedAccess>]
module ChangeCurrentQuestion =

    type QuestionData = { Question: QuestionNumber }
    type Command = WithinQuizCommand<QuestionData>
    type WorkflowDeprecated = Command -> Result<CurrentQuestionChanged, QuizStateError>
    type Workflow = Command -> AsyncResult<CurrentQuestionChanged, QuizStateError>
