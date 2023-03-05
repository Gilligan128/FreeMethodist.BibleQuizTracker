module FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open Microsoft.FSharp.Core

type UpdatedQuiz =
    { QuizState: RunningQuiz
      RevertedAnswer: RevertedCorrectAnswer }

type UpdateQuiz = Quizzer -> RunningQuiz -> Result<UpdatedQuiz, AnswerIncorrectly.Error>
type CreateEvents = UpdatedQuiz -> AnswerIncorrectly.Event list


let private revertScoressBasedOnQuizzerIfNecessary revertedCorrectAnswer quiz =
    revertedCorrectAnswer
    |> RevertedCorrectAnswer.toOption
    |> Option.bind (fun quizzer -> RunningQuiz.updateScoresBasedOnQuizzer (QuizScore.revertCorrectAnswer) quizzer quiz)
    |> Option.defaultValue quiz

let updateQuiz: UpdateQuiz =

    let updateOrAddQuestion quizzer questionNumber questionOpt =
        questionOpt |> QuizAnswer.answerIncorrectly quizzer questionNumber

    fun quizzer quiz ->
        result {
            let quizCurrentQuestion = quiz.CurrentQuestion

            let currentAnswerRecord =
                quiz.Questions.TryFind quizCurrentQuestion
                |> Option.map (fun q -> q.AnswerState)

            let! changedQuestion, revertedCorrectAnswer =
                updateOrAddQuestion quizzer quizCurrentQuestion currentAnswerRecord
                |> Result.mapError AnswerIncorrectly.Error.QuizzerAlreadyAnsweredIncorrectly

            let quizState =
                { quiz with
                    CurrentQuizzer = None
                    Questions = RunningQuiz.changeCurrentAnswer quiz changedQuestion }

            return
                { QuizState = quizState
                  RevertedAnswer = revertedCorrectAnswer }
        }

let answerIncorrectly getQuiz saveQuiz : AnswerIncorrectly.Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.mapError AnswerIncorrectly.DbError

            let! runningQuiz =
                validateRunningQuiz quiz
                |> AsyncResult.ofResult
                |> AsyncResult.mapError AnswerIncorrectly.QuizState

            let! validQuizzer =
                validateCurrentQuizzer runningQuiz
                |> Result.mapError AnswerIncorrectly.Error.NoCurrentQuizzer
                |> AsyncResult.ofResult

            let! updatedQuiz =
                updateQuiz validQuizzer runningQuiz
                |> Result.map (fun updatedQuiz ->
                    { updatedQuiz with QuizState = updatedQuiz.QuizState |> Score.updateQuizScores })
                |> AsyncResult.ofResult

            do!
                updatedQuiz.QuizState
                |> Quiz.Running
                |> saveQuiz
                |> AsyncResult.mapError AnswerIncorrectly.Error.DbError

            let events =
                updatedQuiz.QuizState
                |> ScoreEvents.createScoreEvents updatedQuiz.QuizState
                |> List.map (fun e ->
                    match e with
                    | ScoreEvents.IndividualScoreChanged e -> AnswerIncorrectly.Event.IndividualScoreChanged e
                    | ScoreEvents.TeamScoreChanged e -> AnswerIncorrectly.Event.TeamScoreChanged e)

            return events
        }
