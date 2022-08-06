module FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
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

            let currentAnswerRecord =
                quiz.Questions.TryFind quizCurrentQuestion
                |> Option.map (fun q -> q.AnswerState)

            let! changedQuestion, revertedCorrectAnswer =
                updateOrAddQuestion quizzer quizCurrentQuestion currentAnswerRecord
                |> Result.mapError AnswerIncorrectly.Error.QuizzerAlreadyAnsweredIncorrectly

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

            let revertTeamScoreIfRevertedQuizzerOnTeam revertedAnswer (team: QuizTeamState) =
                revertedAnswer
                |> RevertedCorrectAnswer.toOption
                |> Option.bind (fun q ->
                    team.Quizzers
                    |> List.tryFind (QuizzerState.isQuizzer q))
                |> Option.map (fun _ -> { team with Score = team.Score |> TeamScore.revertCorrectAnswer })
                |> Option.defaultValue team

            return
                { QuizState =
                    { quiz with
                        CurrentQuizzer = None
                        TeamOne =
                            (updateQuizzerInTeamIfFound quizzer quiz.TeamOne)
                            |> revertTeamScoreIfRevertedQuizzerOnTeam revertedCorrectAnswer
                        TeamTwo =
                            (updateQuizzerInTeamIfFound quizzer quiz.TeamTwo)
                            |> revertTeamScoreIfRevertedQuizzerOnTeam revertedCorrectAnswer
                        Questions = RunningTeamQuiz.changeCurrentAnswer quiz changedQuestion }
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
                  |> AnswerIncorrectly.Event.IndividualScoreChanged
                  { Quiz = quiz.QuizState.Code
                    NewScore =
                      quiz.QuizState
                      |> RunningTeamQuiz.getTeam team
                      |> fun team -> team.Score
                    Team = team }
                  |> AnswerIncorrectly.Event.TeamScoreChanged ])
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
                |> AsyncResult.ofResult

            do!
                updatedQuiz.QuizState
                |> Running
                |> saveQuiz
                |> AsyncResult.mapError AnswerIncorrectly.Error.DbError

            return createEvents updatedQuiz
        }
