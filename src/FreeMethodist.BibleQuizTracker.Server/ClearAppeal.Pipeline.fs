module FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Pipeline

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

type UpdateQuiz = RunningQuiz -> Result<RunningQuiz * TeamPosition option, ClearAppeal.Error>

type CreateEvents = RunningQuiz * TeamPosition option -> ClearAppeal.Event list

let updateAppealScore quizzer (quiz: RunningQuiz) =
    let quizzerAndTeamOpt =
        quizzer |> RunningQuiz.tryFindQuizzer quiz

    quizzerAndTeamOpt
    |> Option.bind (function
        | _, Some team ->
            quiz
            |> RunningQuiz.updateTeamScore QuizScore.revertAppealFailure team
            |> Some
        | quizzer, None ->
            quiz
            |> RunningQuiz.updateScoresBasedOnQuizzer QuizScore.revertAppealFailure quizzer.Name)

let updateQuiz: UpdateQuiz =
    fun quiz ->
        result {
            let! changedQuestion, revertingAppealer =
                quiz.Questions
                |> Map.tryFind quiz.CurrentQuestion
                |> Option.defaultValue QuestionState.initial
                |> fun original ->
                    match original.FailedAppeals with
                    | failure :: _ -> Ok({ original with FailedAppeal = None; FailedAppeals = [] }, failure)
                    | [] -> Error ClearAppeal.Error.NoFailedAppeal

            let updateCurrentQuestion changedQuestion quiz =
                changedQuestion
                |> fun changedQuestion ->
                    { quiz with
                        Questions =
                            quiz.Questions
                            |> Map.add quiz.CurrentQuestion changedQuestion }

            let revertedTeamOpt =
                quiz
                |> RunningQuiz.tryFindQuizzer2 revertingAppealer
                |> Option.bind snd

            let updatedQuiz =
                quiz
                |> updateCurrentQuestion changedQuestion
                |> (fun quiz ->
                    quiz
                    |> updateAppealScore revertingAppealer
                    |> Option.defaultValue quiz)

            return updatedQuiz, revertedTeamOpt
        }

let createEvents: CreateEvents =
    fun (quiz, revertedTeamOpt) ->

        let revertedEvents =
            revertedTeamOpt
            |> Option.map (fun team ->
                [ ClearAppeal.Event.TeamScoreChanged
                      { Quiz = quiz.Code
                        Team = team
                        NewScore = quiz |> RunningQuiz.getTeamScore team } ])
            |> Option.defaultValue []

        revertedEvents

let clearAppeal getQuiz saveQuiz : ClearAppeal.Workflow =
    fun command ->
        asyncResult {
            let! quiz =
                getQuiz command.Quiz
                |> AsyncResult.mapError ClearAppeal.DbError

            let! validQuiz =
                validateRunningQuiz quiz
                |> Result.mapError ClearAppeal.Error.QuizState
                |> AsyncResult.ofResult

            let! updatedQuiz, revertedTeam = updateQuiz validQuiz |> AsyncResult.ofResult

            do!
                updatedQuiz
                |> Running
                |> saveQuiz
                |> AsyncResult.mapError ClearAppeal.Error.DbError

            return createEvents (updatedQuiz, revertedTeam)
        }
