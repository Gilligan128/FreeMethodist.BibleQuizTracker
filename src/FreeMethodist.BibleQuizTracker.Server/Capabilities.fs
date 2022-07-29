namespace  FreeMethodist.BibleQuizTracker.Server.Capabilities

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow
open FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Workflow
open FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow
open FreeMethodist.BibleQuizTracker.Server.RemoveQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.SelectQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.AddQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.FailAppeal.Workflow

module Capabilities =
    type RunQuizCapabilityProvider = {
        AddQuizzer : User -> AddQuizzer.Workflow
        RemoveQuizzer: User -> RemoveQuizzer.Workflow
        AnswerCorrectly : User -> Quizzer option -> AnswerCorrectly.Workflow
        AnswerIncorrectly : User -> Quizzer option -> AnswerIncorrectly.Workflow
        FailAppeal : User -> Quizzer option -> FailAppeal.Workflow
        ClearAppeal : User -> Quizzer option -> ClearAppeal.Workflow
        ChangeCurrentQuestion : User -> ChangeCurrentQuestion.Workflow
        SelectQuizzer: User -> SelectQuizzer.Workflow
    }