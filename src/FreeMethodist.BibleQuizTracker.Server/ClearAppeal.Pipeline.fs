module FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Pipeline

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

type UpdateQuiz = RunningQuiz -> Result<RunningQuiz * TeamPosition list, ClearAppeal.Error>

type CreateEvents = RunningQuiz * TeamPosition list -> ClearAppeal.Event list

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
            let! changedQuestion, revertingAppealers =
                quiz.Questions
                |> Map.tryFind quiz.CurrentQuestion
                |> Option.defaultValue QuestionState.initial
                |> fun original ->
                    match original.FailedAppeals with
                    | [] -> Error ClearAppeal.Error.NoFailedAppeal
                    | failure -> Ok({ original with FailedAppeals = [] }, failure)

            let updateCurrentQuestion changedQuestion quiz =
                changedQuestion
                |> fun changedQuestion ->
                    { quiz with
                        Questions =
                            quiz.Questions
                            |> Map.add quiz.CurrentQuestion changedQuestion }

            let revertedTeams =
                revertingAppealers
                |> List.choose (fun quizzer ->
                    quiz
                    |> RunningQuiz.tryFindQuizzer2 quizzer
                    |> Option.bind snd)
                |> List.distinct

            let updatedQuiz =
                quiz
                |> updateCurrentQuestion changedQuestion
                |> (fun quiz ->
                    revertingAppealers
                    |> List.fold
                        (fun quiz quizzer ->
                            quiz
                            |> updateAppealScore quizzer
                            |> Option.defaultValue quiz)
                        quiz)

            return updatedQuiz, revertedTeams
        }

let createEvents: CreateEvents =
    fun (quiz, revertedTeams) ->

        let revertedEvents =
            revertedTeams
            |> List.map (fun team ->
                ClearAppeal.Event.TeamScoreChanged
                    { Quiz = quiz.Code
                      Team = team
                      NewScore = quiz |> RunningQuiz.getTeamScore team })

        revertedEvents


let clearAppealPureWorkflow quiz _ =
    asyncResult {
        let! validQuiz =
            validateRunningQuiz quiz
            |> Result.mapError ClearAppeal.Error.QuizState
            |> AsyncResult.ofResult

        let! updatedQuiz, revertedTeam = updateQuiz validQuiz |> AsyncResult.ofResult

        let events =
            createEvents (updatedQuiz, revertedTeam)

        return updatedQuiz, events
    }

let clearAppeal runQuizWorklfowEngine : ClearAppeal.Workflow =
    fun command ->
        command
        |> runQuizWorklfowEngine clearAppealPureWorkflow ClearAppeal.DbError
