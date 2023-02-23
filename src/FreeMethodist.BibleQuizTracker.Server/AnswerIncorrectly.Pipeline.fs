module FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline

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
        questionOpt
        |> QuizAnswer.answerIncorrectly quizzer questionNumber

    fun quizzer quiz ->
        result {
            let quizCurrentQuestion =
                quiz.CurrentQuestion

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
                |> revertScoressBasedOnQuizzerIfNecessary revertedCorrectAnswer
            
            return
                { QuizState = quizState
                  RevertedAnswer = revertedCorrectAnswer }
        }

let createEvents: CreateEvents =
    fun quiz ->
        let revertedQuizzerOpt =
            quiz.RevertedAnswer
            |> RevertedCorrectAnswer.toOption
            |> Option.bind (RunningQuiz.tryFindQuizzer quiz.QuizState)

        let revertedEvents =
            revertedQuizzerOpt
            |> Option.map (fun (quizzer, teamOpt) ->
                [ { Quiz = quiz.QuizState.Code
                    Quizzer = quizzer.Name
                    NewScore = quizzer.Score
                    Question = quiz.QuizState.CurrentQuestion }
                  |> AnswerIncorrectly.Event.IndividualScoreChanged ]
                @ (teamOpt
                   |> Option.map (fun team ->
                       { Quiz = quiz.QuizState.Code
                         NewScore =
                           quiz.QuizState
                           |> RunningQuiz.getTeam team
                           |> fun team -> team.Score
                         Team = team }
                       |> AnswerIncorrectly.Event.TeamScoreChanged)
                   |> Option.toList))
            |> Option.defaultValue []

        let quizzerChanged =
            { Quiz = quiz.QuizState.Code
              CurrentQuizzer = quiz.QuizState.CurrentQuizzer }
            |> AnswerIncorrectly.Event.CurrentQuizzerChanged

        [ quizzerChanged
          yield! revertedEvents ]

let answerIncorrectly getQuiz saveQuiz : AnswerIncorrectly.Workflow =
    fun command ->
        asyncResult {
            let! quiz =
                getQuiz command.Quiz
                |> AsyncResult.mapError AnswerIncorrectly.DbError

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
                |> AsyncResult.ofResult

            do!
                updatedQuiz.QuizState
                |> Quiz.Running
                |> saveQuiz
                |> AsyncResult.mapError AnswerIncorrectly.Error.DbError

            return createEvents updatedQuiz
        }
