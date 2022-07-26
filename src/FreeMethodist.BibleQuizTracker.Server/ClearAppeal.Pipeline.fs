module FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Pipeline

open FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Workflow
open FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Workflow.ClearAppeal
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

type UpdateQuiz = RunningTeamQuiz -> Result<RunningTeamQuiz * TeamPosition option, ClearAppeal.Error>

type CreateEvents = RunningTeamQuiz * TeamPosition option -> ClearAppeal.Event list

let updateQuiz: UpdateQuiz =
    fun quiz ->
        result {
            let! changedQuestion, revertingAppealer =
                quiz.Questions
                |> Map.tryFind quiz.CurrentQuestion
                |> Option.defaultValue QuestionState.initial
                |> fun original ->
                    match original.FailedAppeal with
                    | Some failed -> Ok({ original with FailedAppeal = None }, failed)
                    | None -> Error ClearAppeal.Error.NoFailedAppeal


            let updateCurrentQuestion changedQuestion quiz =
                changedQuestion
                |> fun changedQuestion ->
                    { quiz with
                        Questions =
                            quiz.Questions
                            |> Map.add quiz.CurrentQuestion changedQuestion }

            let updateRevertingTeamScore teamPositionOpt (quiz: RunningTeamQuiz) =
                let updateScore (team: QuizTeamState) =
                    { team with Score = team.Score |> TeamScore.revertAppealFailure }

                match teamPositionOpt with
                | None -> quiz
                | Some TeamOne -> { quiz with TeamOne = updateScore quiz.TeamOne }
                | Some TeamTwo -> { quiz with TeamTwo = updateScore quiz.TeamTwo }

            let revertedTeamOpt =
                revertingAppealer
                |> fun q -> RunningTeamQuiz.tryFindQuizzerAndTeam q quiz
                |> Option.map snd

            let updatedQuiz =
                quiz
                |> updateCurrentQuestion changedQuestion
                |> updateRevertingTeamScore revertedTeamOpt

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
                        NewScore = quiz |> RunningTeamQuiz.getTeamScore team } ])
            |> Option.defaultValue []

        revertedEvents

let clearAppeal getQuiz saveQuiz : Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.mapError DbError

            let! validQuiz =
                validateQuiz quiz
                |> Result.mapError Error.QuizState
                |> AsyncResult.ofResult

            let! updatedQuiz, revertedTeam =
                updateQuiz validQuiz
                |> AsyncResult.ofResult

            do! updatedQuiz |> Running |> saveQuiz |> AsyncResult.mapError ClearAppeal.Error.DbError 

            return createEvents (updatedQuiz, revertedTeam)
        }
