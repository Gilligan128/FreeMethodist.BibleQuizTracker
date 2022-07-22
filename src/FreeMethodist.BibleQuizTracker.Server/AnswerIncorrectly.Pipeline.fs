module FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline

open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline
open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow.AnswerIncorrectly
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open Microsoft.FSharp.Core

type UpdatedQuiz =
    { QuizState: RunningTeamQuiz
      RevertedAnswer: RevertedCorrectAnswer }

type UpdateQuiz = Quizzer -> RunningTeamQuiz -> Result<UpdatedQuiz, AnswerIncorrectly.Error>
type CreateEvents = UpdatedQuiz -> AnswerIncorrectly.Event list

let updateQuiz: UpdateQuiz =

    let updateOrAddQuestion quizzer questionNumber questionOpt =
        questionOpt
        |> QuizAnswer.answerIncorrectly quizzer questionNumber

    fun quizzer quiz ->
        result {
            let quizCurrentQuestion =
                quiz.CurrentQuestion

            let currentQuestionRecord =
                quiz.QuestionsDeprecated.TryFind quizCurrentQuestion

            let! changedQuestion, revertedCorrectAnswer =
                updateOrAddQuestion quizzer quizCurrentQuestion currentQuestionRecord
                |> Result.mapError Error.QuizzerAlreadyAnsweredIncorrectly

            let updateScore revertedAnswer (quizzer: QuizzerState) =
                match revertedAnswer with
                | Reverted q -> quizzer.Score |> TeamScore.revertCorrectAnswer
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
                { QuizState =
                    { quiz with
                        CurrentQuizzer = None
                        TeamOne = updateQuizzerInTeamIfFound quizzer quiz.TeamOne
                        TeamTwo = updateQuizzerInTeamIfFound quizzer quiz.TeamTwo
                        QuestionsDeprecated =
                            quiz.QuestionsDeprecated
                            |> Map.add quizCurrentQuestion changedQuestion
                        Questions =
                            quiz.Questions
                            |> Map.change quizCurrentQuestion (fun q ->
                                q
                                |> Option.defaultValue (QuestionState.create changedQuestion)
                                |> Some) }
                  RevertedAnswer = revertedCorrectAnswer }
        }

let createEvents: CreateEvents =
    fun quiz ->
        let revertedQuizzerOpt =
            quiz.RevertedAnswer
            |> RevertedCorrectAnswer.toOption
            |> Option.map (fun reverted -> RunningTeamQuiz.findQuizzerAndTeam reverted quiz.QuizState)

        let revertedEvents =
            revertedQuizzerOpt
            |> Option.map (fun (quizzer, team) ->
                [ { Quiz = quiz.QuizState.Code
                    Quizzer = quizzer.Name
                    NewScore = quizzer.Score
                    Question = quiz.QuizState.CurrentQuestion }
                  |> Event.IndividualScoreChanged
                  { Quiz = quiz.QuizState.Code
                    NewScore =
                      quiz.QuizState
                      |> RunningTeamQuiz.getTeam team
                      |> fun team -> team.Score
                    Team = team }
                  |> Event.TeamScoreChanged ])
            |> Option.defaultValue []

        let quizzerChanged =
            { Quiz = quiz.QuizState.Code
              CurrentQuizzer = quiz.QuizState.CurrentQuizzer }
            |> Event.CurrentQuizzerChanged

        [ quizzerChanged
          yield! revertedEvents ]

let answerIncorrectly getQuiz saveQuiz : Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.ofAsync

            let! runningQuiz =
                validateQuiz quiz
                |> AsyncResult.ofResult
                |> AsyncResult.mapError QuizState

            let! validQuizzer =
                validateCurrentQuizzer runningQuiz
                |> Result.mapError Error.NoCurrentQuizzer
                |> AsyncResult.ofResult

            let! updatedQuiz =
                updateQuiz validQuizzer runningQuiz
                |> AsyncResult.ofResult

            do!
                updatedQuiz.QuizState
                |> Running
                |> saveQuiz
                |> AsyncResult.ofAsync

            return createEvents updatedQuiz
        }
