module FreeMethodist.BibleQuizTracker.Server.FailAppeal.Pipeline

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

type UpdateQuiz =
    Quizzer * TeamPosition -> RunningTeamQuiz -> Result<RunningTeamQuiz * TeamPosition option, FailAppeal.Error>

type CreateEvents = TeamPosition -> RunningTeamQuiz * TeamPosition option -> FailAppeal.Event list

let updateQuiz: UpdateQuiz =
    fun (currentQuizzer, teamPosition) quiz ->
        result {
            let! changedQuestion, revertingAppealer =
                quiz.Questions
                |> Map.tryFind quiz.CurrentQuestion
                |> Option.defaultValue QuestionState.initial
                |> fun original ->
                    match original.FailedAppeal with
                    | Some failure when failure = currentQuizzer ->
                        Error(FailAppeal.Error.AppealAlreadyFailed currentQuizzer)
                    | _ -> Ok({ original with FailedAppeal = Some currentQuizzer }, original.FailedAppeal)



            let updateCurrentQuestion changedQuestion quiz =
                changedQuestion
                |> fun changedQuestion ->
                    { quiz with
                        Questions =
                            quiz.Questions
                            |> Map.add quiz.CurrentQuestion changedQuestion }

            let updateFailingTeamScore teamPosition (quiz: RunningTeamQuiz) =
                let updateScore (team: QuizTeamState) =
                    { team with Score = team.Score |> TeamScore.failAppeal }

                match teamPosition with
                | TeamOne -> { quiz with TeamOne = updateScore quiz.TeamOne }
                | TeamTwo -> { quiz with TeamTwo = updateScore quiz.TeamTwo }

            let updateRevertedAppealTeamScore teamPositionOpt (quiz: RunningTeamQuiz) =
                let updateScore (team: QuizTeamState) =
                    { team with Score = team.Score |> TeamScore.revertAppealFailure }

                match teamPositionOpt with
                | Some TeamOne -> { quiz with TeamOne = updateScore quiz.TeamOne }
                | Some TeamTwo -> { quiz with TeamTwo = updateScore quiz.TeamTwo }
                | None -> quiz

            let revertedQuizzerOpt =
                revertingAppealer
                |> Option.bind (RunningTeamQuiz.tryFindQuizzerAndTeam (quiz.TeamOne, quiz.TeamTwo))
                |> Option.map snd

            let updatedQuiz =
                quiz
                |> updateCurrentQuestion changedQuestion
                |> updateFailingTeamScore teamPosition
                |> updateRevertedAppealTeamScore revertedQuizzerOpt

            return updatedQuiz, revertedQuizzerOpt
        }

let createEvents: CreateEvents =
    fun failingTeam (quiz, revertedTeamOpt) ->
        let failingEvents =
            FailAppeal.Event.TeamScoreChanged
                { Quiz = quiz.Code
                  Team = failingTeam
                  NewScore = RunningTeamQuiz.getTeamScore failingTeam quiz }
            |> fun e -> [ e ]

        let revertedEvents =
            revertedTeamOpt
            |> Option.map (fun team ->
                [ FailAppeal.Event.TeamScoreChanged
                      { Quiz = quiz.Code
                        Team = team
                        NewScore = quiz |> RunningTeamQuiz.getTeamScore team } ])
            |> Option.defaultValue []

        failingEvents @ revertedEvents

let failAppeal getQuiz saveQuiz : FailAppeal.Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.mapError FailAppeal.DbError

            let! validQuiz =
                validateRunningQuiz quiz
                |> Result.mapError FailAppeal.Error.QuizState
                |> AsyncResult.ofResult

            let! quizzer, team =
                validateCurrentQuizzerWithTeam validQuiz
                |> AsyncResult.ofResult
                |> AsyncResult.mapError FailAppeal.Error.NoCurrentQuizzer

            let! updatedQuiz, revertedTeam =
                updateQuiz (quizzer, team) validQuiz
                |> AsyncResult.ofResult

            do! updatedQuiz |> Running |> saveQuiz |> AsyncResult.mapError FailAppeal.Error.DbError

            return createEvents team (updatedQuiz, revertedTeam)
        }
