module FreeMethodist.BibleQuizTracker.Server.RemoveQuizzer_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Pipeline
open FreeMethodist.BibleQuizTracker.Server.RemoveQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

type ValidateRemoval = TeamQuiz -> RemoveQuizzer.Data -> Result<RunningTeamQuiz, RemoveQuizzer.Error>
type RemoveQuizzerFromQuiz = RunningTeamQuiz -> RemoveQuizzer.Data -> RunningTeamQuiz
type CreateEvent = QuizCode -> RemoveQuizzer.Data -> QuizzerNoLongerParticipating

let validateRemoval validateQuiz : ValidateRemoval =
    fun quiz input ->

        result {
            let! validQuiz =
                validateQuiz quiz
                |> Result.mapError RemoveQuizzer.QuizStateError

            let teamHasQuizzer team =
                if team.Quizzers
                   |> List.map (fun q -> q.Name)
                   |> List.contains input.Quizzer then
                    Ok validQuiz
                else
                    Error(RemoveQuizzer.QuizzerNotParticipating input.Quizzer)

            return!
                match input.Team with
                | TeamOne -> teamHasQuizzer validQuiz.TeamOne
                | TeamTwo -> teamHasQuizzer validQuiz.TeamTwo
        }

let removeQuizzerFromQuiz: RemoveQuizzerFromQuiz =
    fun quiz input ->
        let remove (team: QuizTeamState) =
            { team with
                Quizzers =
                    team.Quizzers
                    |> List.filter (fun q -> q.Name <> input.Quizzer) }

        match input.Team with
        | TeamOne -> { quiz with TeamOne = remove quiz.TeamOne }
        | TeamTwo -> { quiz with TeamTwo = remove quiz.TeamTwo }

let createEvent: CreateEvent =
    fun quizCode input ->
        { Quiz = quizCode
          Quizzer = input.Quizzer }


let removeQuizzer getQuiz (saveQuiz: SaveTeamQuiz) : RemoveQuizzer.Workflow =
    fun command ->
        let quiz = getQuiz command.Quiz

        result {
            let! validQuiz = validateRemoval validateQuiz quiz command.Data

            removeQuizzerFromQuiz validQuiz command.Data
            |> Running
            |> saveQuiz

            return createEvent command.Quiz command.Data
        }
