module FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline

open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow.AnswerCorrectly
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core

//Answer Question
type AnswerQuestion = Quizzer -> AnsweredQuestion
type UpdateQuiz = Quizzer -> RunningTeamQuiz -> Result<RunningTeamQuiz, AnswerCorrectly.Error>

type CreateEvents = RunningTeamQuiz -> RunningTeamQuiz -> AnswerCorrectly.Event list

let updateQuiz: UpdateQuiz =
    let quizzerCorrectlyAnswers (quizzer: QuizzerState) =
        { quizzer with Score = quizzer.Score |> TeamScore.correctAnswer }

    let updateQuizTeams (quiz: RunningTeamQuiz) teamOne teamTwo =
        { quiz with
            TeamOne = teamOne
            TeamTwo = teamTwo
            CurrentQuestion = quiz.CurrentQuestion |> PositiveNumber.increment }

    let updateAnweringQuizzer isQuizzer quizzers =
        quizzers
        |> List.map (fun q ->
            if isQuizzer q then
                (quizzerCorrectlyAnswers q)
            else
                q)

    fun quizzerName quiz ->
        let isQuizzer (quizzer: QuizzerState) = quizzer.Name = quizzerName

        let updateTeamOpt team =
            quiz.TeamOne.Quizzers
            |> List.exists isQuizzer
            |> fun found -> if found then Some team else None
            |> Option.map (fun team -> { team with Quizzers = team.Quizzers |> updateAnweringQuizzer isQuizzer })
            |> Option.map (fun team -> { team with Score = team.Score |> TeamScore.correctAnswer })

        Option.map2 (updateQuizTeams quiz) (updateTeamOpt quiz.TeamOne) (updateTeamOpt quiz.TeamTwo)
        |> Result.ofOption (QuizzerNotFound quizzerName)
