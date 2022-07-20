module FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline

open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow.AnswerIncorrectly
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open Microsoft.FSharp.Core

type ValidCurrentQuizzer = private ValidCurrentQuizzer of Quizzer
type ValidateCurrentQuizzer = RunningTeamQuiz -> Result<ValidCurrentQuizzer, AnswerIncorrectly.Error>
type UpdateQuiz = ValidCurrentQuizzer -> RunningTeamQuiz -> Result<RunningTeamQuiz, AnswerIncorrectly.Error>
type CreateEvents = RunningTeamQuiz -> AnswerIncorrectly.Event list

let validateQuizzer: ValidateCurrentQuizzer =
    fun quiz ->
        quiz.CurrentQuizzer
        |> Option.map ValidCurrentQuizzer
        |> (Result.ofOption Error.NoCurrentQuizzer)

let updateQuiz: UpdateQuiz =

    let updateOrAddQuestion quizzer questionNumber questionOpt =
        questionOpt
        |> QuizQuestion.answerIncorrectly quizzer questionNumber

    fun (ValidCurrentQuizzer quizzer) quiz ->
        result {
            let quizCurrentQuestion =
                quiz.CurrentQuestion

            let currentQuestionRecord =
                quiz.Questions.TryFind quizCurrentQuestion

            let! changedQuestion, revertedCorrectAnswer =
                updateOrAddQuestion quizzer quizCurrentQuestion currentQuestionRecord
                |> Result.mapError Error.QuizzerAlreadyAnsweredIncorrectly

            let updateScore revertedAnswer (quizzer: QuizzerState) =
                match revertedAnswer with
                | Reverted -> quizzer.Score |> TeamScore.revertCorrectAnswer
                | NoChange -> quizzer.Score

            let updateQuizzerWithScore revertedAnswer (quizzer: QuizzerState) =
                { quizzer with Score = updateScore revertedAnswer quizzer }

            let updateQuizzerInTeamIfFound quizzer (team: QuizTeamState) =
                { team with
                    Quizzers =
                        team.Quizzers
                        |> List.map (fun q ->
                            if q.Name = quizzer then
                                (updateQuizzerWithScore revertedCorrectAnswer) q
                            else
                                q) }

            return
                { quiz with
                    CurrentQuizzer = None
                    TeamOne = updateQuizzerInTeamIfFound quizzer quiz.TeamOne
                    TeamTwo = updateQuizzerInTeamIfFound quizzer quiz.TeamTwo
                    Questions =
                        quiz.Questions
                        |> Map.add quizCurrentQuestion changedQuestion }
        }

let createEvents: CreateEvents =
    fun quiz ->
        let quizzerChanged =
            { Quiz = quiz.Code
              CurrentQuizzer = quiz.CurrentQuizzer }
            |> Event.CurrentQuizzerChanged

        [ quizzerChanged ]

let answerIncorrectly getQuiz saveQuiz : Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.ofAsync

            let! runningQuiz =
                validateQuiz quiz
                |> AsyncResult.ofResult
                |> AsyncResult.mapError QuizState

            let! validQuizzer =
                validateQuizzer runningQuiz
                |> AsyncResult.ofResult

            let! updatedQuiz =
                updateQuiz validQuizzer runningQuiz
                |> AsyncResult.ofResult

            do!
                updatedQuiz
                |> Running
                |> saveQuiz
                |> AsyncResult.ofAsync

            return createEvents updatedQuiz
        }
