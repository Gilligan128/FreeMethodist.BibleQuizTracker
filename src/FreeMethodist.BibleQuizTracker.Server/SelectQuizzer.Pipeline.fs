module FreeMethodist.BibleQuizTracker.Server.SelectQuizzer_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.SelectQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

type ValidateSelection = Quiz -> SelectQuizzer.Input -> Result<RunningTeamQuiz, SelectQuizzer.Error>
type ChangeCurrentQuizzer = Quizzer -> RunningTeamQuiz -> RunningTeamQuiz
type CreateEvent = RunningTeamQuiz -> Quizzer -> CurrentQuizzerChanged


let validateSelection: ValidateSelection =
    fun teamQuiz input ->
        result {
            let! validQuiz =
                validateQuiz teamQuiz
                |> Result.mapError SelectQuizzer.Error.QuizState

            let hasQuizzer =
                validQuiz.TeamOne.Quizzers
                @ validQuiz.TeamTwo.Quizzers
                |> List.exists (fun q -> q.Name = input.Quizzer)

            do!
                if hasQuizzer then
                    Ok()
                else
                    Error(SelectQuizzer.Error.QuizzerNotParticipating input.Quizzer)

            return!
                match validQuiz.CurrentQuizzer with
                | None -> Ok validQuiz
                | Some currentQuizzer ->
                    if currentQuizzer = input.Quizzer then
                        Error SelectQuizzer.QuizzerAlreadyCurrent
                    else
                        Ok validQuiz
        }

let changeCurrentQuizzer: ChangeCurrentQuizzer =
    fun quizzer quiz -> { quiz with CurrentQuizzer = Some quizzer }

let createEvent: CreateEvent =
    fun quiz quizzer -> { Quiz = quiz.Code; CurrentQuizzer = Some quizzer }

let selectQuizzer getQuiz (saveQuiz: SaveTeamQuizAsync) : SelectQuizzer.Workflow =
    fun command ->
        let validateSelection quiz = validateSelection quiz command.Data |> AsyncResult.ofResult

        asyncResult {
            let! validQuiz = getQuiz command.Quiz |> AsyncResult.ofAsync |> AsyncResult.bind validateSelection

            do! validQuiz
                |> changeCurrentQuizzer command.Data.Quizzer
                |> Running
                |> saveQuiz |> AsyncResult.mapError SelectQuizzer.DbError

            return createEvent validQuiz command.Data.Quizzer
        } 
