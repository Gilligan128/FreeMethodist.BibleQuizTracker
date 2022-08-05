module FreeMethodist.BibleQuizTracker.Server.AddQuizzer_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open Microsoft.FSharp.Core

type ValidateQuizzerAdd = Quiz -> AddQuizzer.Data -> Result<RunningTeamQuiz, AddQuizzer.Error>
type AddQuizzerToQuiz = RunningTeamQuiz -> AddQuizzer.Data -> RunningTeamQuiz
type CreateEvent = QuizCode -> AddQuizzer.Data -> QuizzerParticipating

let validateQuizzer participatingQuizzers quizzer =
    if participatingQuizzers |> Seq.contains quizzer then
        Error(AddQuizzer.Error.QuizzerAlreadyAdded quizzer)
    else
        Ok()

let validateQuizzerAdd (validateQuiz: ValidateQuizIsRunning) : ValidateQuizzerAdd =
    fun teamQuiz input ->
        result {
            let! quiz =
                validateQuiz teamQuiz
                |> Result.mapError AddQuizzer.Error.QuizState

            let getQuizzerNames (quizzers: List<QuizzerState>) =
                quizzers
                |> List.map (fun q -> q.Name)
                |> List.toSeq

            let participatingQuizzers =
                match input.Team with
                | TeamOne -> quiz.TeamOne.Quizzers |> getQuizzerNames
                | TeamTwo -> quiz.TeamTwo.Quizzers |> getQuizzerNames

            do! validateQuizzer participatingQuizzers input.Name
            return quiz
        }

let addQuizzerToQuiz: AddQuizzerToQuiz =
    fun quiz input ->
        let newTeamState (originalTeamState: QuizTeamState) =
            { originalTeamState with
                Quizzers =
                    originalTeamState.Quizzers
                    @ [ { Name = input.Name
                          Participation = ParticipationState.In
                          Score = TeamScore.initial } ] }

        match input.Team with
        | TeamOne -> { quiz with TeamOne = newTeamState quiz.TeamOne }
        | TeamTwo -> { quiz with TeamTwo = newTeamState quiz.TeamTwo }

let createEvent: CreateEvent =
    fun code input -> { Quizzer = input.Name; Quiz = code }

let addQuizzerAsync getQuiz (saveQuiz: SaveQuiz) : AddQuizzer.Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.mapError AddQuizzer.DbError

            let! validQuiz =
                validateQuizzerAdd (validateQuiz) quiz command.Data
                |> Async.retn

            return!
                addQuizzerToQuiz validQuiz command.Data
                |> Running
                |> saveQuiz
                |> AsyncResult.mapError AddQuizzer.Error.DbError

            return createEvent command.Quiz command.Data
        }
