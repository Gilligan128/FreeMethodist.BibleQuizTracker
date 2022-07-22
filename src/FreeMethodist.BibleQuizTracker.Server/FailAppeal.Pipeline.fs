module FreeMethodist.BibleQuizTracker.Server.FailAppeal.Pipeline

open FreeMethodist.BibleQuizTracker.Server.FailAppeal.Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

type ValidateQuizzer = Quizzer option -> Result<Quizzer*TeamPosition, NoCurrentQuizzer>
type updateQUiz = Quizzer -> RunningTeamQuiz -> RunningTeamQuiz
type CreateEvents = TeamPosition -> RunningTeamQuiz -> FailAppeal.Event list



