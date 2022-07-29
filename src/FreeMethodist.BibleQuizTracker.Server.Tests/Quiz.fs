module FreeMethodist.BibleQuizTracker.Server.Tests.Quiz

open FreeMethodist.BibleQuizTracker.Server.Workflow

let insertCurrentAnswer answer quiz =
    { quiz with
        Questions =
            quiz.Questions
            |> Map.change quiz.CurrentQuestion (QuestionState.changeAnswer answer) }
    
let insertAnswer question answer quiz =
    { quiz with
        Questions =
            quiz.Questions
            |> Map.change question (QuestionState.changeAnswer answer) }

