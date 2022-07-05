module FreeMethodist.BibleQuizTracker.Server.NextQuestion_Workflow

open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain

type QuestionChanged = { Quiz: QuizCode; NewQuestion: QuestionNumber; }
 
[<RequireQualifiedAccess>]
module NextQuestion = 

    type QuestionData = { Question:QuestionNumber }
    type Command = WithinQuizCommand<QuestionData>
    type Workflow = Command -> Result<QuestionChanged, QuizStateError>
    