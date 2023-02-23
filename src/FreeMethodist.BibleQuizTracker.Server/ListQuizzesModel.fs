module FreeMethodist.BibleQuizTracker.Server.ListQuizzesModel

open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Workflow

type QuizItem = { Code : string;  }

type Model = {
    Quizzes : Deferred<Result<QuizItem list, DbError>>
}