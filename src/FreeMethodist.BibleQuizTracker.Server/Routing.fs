module FreeMethodist.BibleQuizTracker.Server.Routing

open Bolero
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.LiveScoreModel

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/quiz/{quizCode}">] QuizDetails of quizCode: string * PageModel<QuizDetailsModel>
    | [<EndPoint "/quiz/{quizCode}/run">] QuizRun of quizCode: string
    | [<EndPoint "/quiz/{quizCode}/spectate">] QuizSpectate of quizCode: string
    | [<EndPoint "/quiz/{quizCode}/live-score">] QuizLiveScore of quizCode: string * PageModel<LiveScoreModel>
    | [<EndPoint "/quiz/completed">] QuizzesCompleted of PageModel<CompletedQuizzesModel.Model>