module FreeMethodist.BibleQuizTracker.Server.FailAppeal.Pipeline

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

type UpdateQuiz = Quizzer * TeamPosition option -> RunningQuiz -> Result<RunningQuiz, FailAppeal.Error>

type CreateEvents = Quizzer * TeamPosition option -> RunningQuiz -> FailAppeal.Event list

let updateAppealScore updateScore (quizzer, teamOpt) quiz =
    match teamOpt with
    | Some teamPosition ->
        quiz
        |> RunningQuiz.updateTeamScore updateScore teamPosition
        |> Some
    | None ->
        quiz
        |> RunningQuiz.updateScoresBasedOnQuizzer updateScore quizzer
    |> Option.defaultValue quiz

let updateQuiz: UpdateQuiz =
    fun (currentQuizzer, teamPositionOpt) quiz ->
        result {
            let! changedQuestion =
                quiz.Questions
                |> Map.tryFind quiz.CurrentQuestion
                |> Option.defaultValue QuestionState.initial
                |> fun original ->
                    if original.FailedAppeals
                       |> List.contains currentQuizzer then
                        Error(FailAppeal.Error.AppealAlreadyFailed currentQuizzer)
                    else
                        Ok(
                            { original with
                                FailedAppeals =
                                    original.FailedAppeals
                                    |> List.append [ currentQuizzer ] }
                        )

            let updateCurrentQuestion changedQuestion quiz =
                changedQuestion
                |> fun changedQuestion ->
                    { quiz with
                        Questions =
                            quiz.Questions
                            |> Map.add quiz.CurrentQuestion changedQuestion }

            let updatedQuiz =
                quiz
                |> updateCurrentQuestion changedQuestion
                |> updateAppealScore QuizScore.failAppeal (currentQuizzer, teamPositionOpt)

            return updatedQuiz
        }

let private createScoreChangeEvents (quizzer, teamOpt) (quiz: RunningQuiz) =
    match teamOpt with
    | Some team ->
        [ FailAppeal.Event.TeamScoreChanged
              { Quiz = quiz.Code
                Team = team
                NewScore =
                  quiz
                  |> RunningQuiz.getTeam team
                  |> fun t -> t.Score } ]
    | None ->
        [ FailAppeal.Event.IndividualScoreChanged
              { Quiz = quiz.Code
                Quizzer = quizzer
                NewScore =
                  (quiz
                   |> RunningQuiz.findQuizzer quizzer
                   |> fst
                   |> fun q -> q.Score)
                Question = quiz.CurrentQuestion } ]

let createEvents: CreateEvents =
    fun (failingQuizzer, failingTeamOpt) quiz ->
        let failingEvents =
            quiz
            |> createScoreChangeEvents (failingQuizzer, failingTeamOpt)

        failingEvents

let failAppeal getQuiz saveQuiz : FailAppeal.Workflow =
    fun command ->
        asyncResult {
            let! quiz =
                getQuiz command.Quiz
                |> AsyncResult.mapError FailAppeal.DbError

            let! validQuiz =
                validateRunningQuiz quiz
                |> Result.mapError FailAppeal.Error.QuizState
                |> AsyncResult.ofResult

            let! quizzerAndTeam =
                validateCurrentQuizzerWithTeam validQuiz
                |> AsyncResult.ofResult
                |> AsyncResult.mapError FailAppeal.Error.NoCurrentQuizzer

            let! updatedQuiz =
                updateQuiz quizzerAndTeam validQuiz
                |> AsyncResult.ofResult

            do!
                updatedQuiz
                |> Running
                |> saveQuiz
                |> AsyncResult.mapError FailAppeal.Error.DbError

            return createEvents quizzerAndTeam updatedQuiz
        }
