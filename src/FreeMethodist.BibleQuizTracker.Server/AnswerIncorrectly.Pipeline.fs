module FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline

open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow.AnswerIncorrectly
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

type ValidQuizzer = private ValidQuizzer of Quizzer
type validateQuizzer = Quizzer option -> RunningTeamQuiz -> ValidQuizzer
type updateQuiz = RunningTeamQuiz
type CreateEvents = ValidQuizzer -> RunningTeamQuiz -> AnswerIncorrectly.Event list


let createEvents : CreateEvents =
    fun (ValidQuizzer validQuizzer) quiz ->
        let quizzerChanged = { Quiz = quiz.Code; Quizzer = Some validQuizzer } |> Event.CurrentQuizzerChanged
        [quizzerChanged]