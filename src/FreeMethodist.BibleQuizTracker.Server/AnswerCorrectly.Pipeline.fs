module FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline

open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

//steps
type DifferentQuestion =
    | Different
    | Same

//Answer Question
type AnswerQuestion = Quizzer -> AnsweredQuestion
type UpdateQuiz = Quizzer -> RunningTeamQuiz -> Result<RunningTeamQuiz, AnswerCorrectly.Error>

type CreateEvents = RunningTeamQuiz -> RunningTeamQuiz -> AnswerCorrectly.Event list 