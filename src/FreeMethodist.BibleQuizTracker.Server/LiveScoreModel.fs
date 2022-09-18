module FreeMethodist.BibleQuizTracker.Server.LiveScoreModel

open System
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Workflow

type LiveScoreQuizzer = { Score: QuizScore; Name: Quizzer }

type LiveScoreTeam =
    { Name: string
      Score: QuizScore
      Quizzers: LiveScoreQuizzer list }

type LiveScoreCompetitionStyle =
    | Individual of LiveScoreQuizzer list
    | Team of LiveScoreTeam * LiveScoreTeam

type LiveScoreQuestionState =
    | Current of QuestionNumber
    | Completed of int

type LiveScores =
    { LastUpdated: DateTimeOffset
      QuestionState: LiveScoreQuestionState
      CompetitionStyle: LiveScoreCompetitionStyle }

type LoadingError =
    | DbError of DbError
    | QuizState of QuizStateError

type LiveScoreModel =
    { Code: QuizCode
      Scores: Deferred<LiveScores> }

