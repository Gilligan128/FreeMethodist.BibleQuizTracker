module FreeMethodist.BibleQuizTracker.Server.Tests.Quiz

open FreeMethodist.BibleQuizTracker.Server.Workflow

let insertCurrentAnswer answer quiz =
    { quiz with
        QuestionsDeprecated =
            quiz.QuestionsDeprecated
            |> Map.add quiz.CurrentQuestion answer
        Questions =
            quiz.Questions
            |> Map.change quiz.CurrentQuestion (QuestionState.changeAnswer answer) }
