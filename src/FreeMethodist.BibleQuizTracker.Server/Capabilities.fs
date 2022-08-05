namespace  FreeMethodist.BibleQuizTracker.Server.Capabilities

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows

module Capabilities =
    type RunQuizCapabilityProvider = {
        AddQuizzer : User -> AddQuizzer.Workflow option
        RemoveQuizzer: User -> RemoveQuizzer.Workflow option
        AnswerCorrectly : User -> Quizzer option -> AnswerCorrectly.Workflow option
        AnswerIncorrectly : User -> Quizzer option -> AnswerIncorrectly.Workflow option
        FailAppeal : User -> Quizzer option -> FailAppeal.Workflow option
        ClearAppeal : User -> Quizzer option -> ClearAppeal.Workflow option
        ChangeCurrentQuestion : User -> ChangeCurrentQuestion.Workflow option
        SelectQuizzer: User -> SelectQuizzer.Workflow option
        CompleteQuiz: User -> CompleteQuiz.Workflow option
    }