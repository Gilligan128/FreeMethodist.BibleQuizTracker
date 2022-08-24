module FreeMethodist.BibleQuizTracker.Server.CompletedQuizzesModel

open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Workflow

type QuizItem = { Name : string;  }

type Model = {
    Quizzes : Deferred<Result<QuizItem list, DbError>>
}