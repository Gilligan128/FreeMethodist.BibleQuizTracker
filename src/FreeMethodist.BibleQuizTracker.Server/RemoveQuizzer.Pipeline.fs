module FreeMethodist.BibleQuizTracker.Server.RemoveQuizzer_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RemoveQuizzer_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

type ValidateRemoval = Quiz -> RemoveQuizzer.Data -> Result<RunningTeamQuiz, RemoveQuizzer.Error>

type RemoveQuizzerFromQuiz =
    RemoveQuizzer.Data -> RunningTeamQuiz -> Jump seq -> RunningTeamQuiz * CurrentQuizzerChanged option

type CreateEvent = QuizCode -> RemoveQuizzer.Data -> QuizzerNoLongerParticipating
type CreateEvents = QuizCode -> RemoveQuizzer.Data -> CurrentQuizzerChanged option -> RemoveQuizzer.Event list

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

let private nextCurrentQuizzer removedQuizzer currentQuizzer =
    if currentQuizzer = removedQuizzer then
        None
    else
        Some currentQuizzer

let removeQuizzerFromQuiz: RemoveQuizzerFromQuiz =
    fun input quiz jumps ->
        let remove (team: QuizTeamState) =
            { team with
                Quizzers =
                    team.Quizzers
                    |> List.filter (fun q -> q.Name <> input.Quizzer) }

        let replaceCurrent =
            quiz.CurrentQuizzer
            |> Option.bind (nextCurrentQuizzer input.Quizzer)

        let newQuiz =
            match input.Team with
            | TeamOne ->
                { quiz with
                    TeamOne = remove quiz.TeamOne
                    CurrentQuizzer = replaceCurrent }
            | TeamTwo ->
                { quiz with
                    TeamTwo = remove quiz.TeamTwo
                    CurrentQuizzer = replaceCurrent }

        let currentQuizzerChanged =
            if newQuiz.CurrentQuizzer <> quiz.CurrentQuizzer then
                Some
                    { Quiz = quiz.Code
                      Quizzer = newQuiz.CurrentQuizzer }
            else
                None

        newQuiz, currentQuizzerChanged


let createEvent: CreateEvent =
    fun quizCode input ->
        { Quiz = quizCode
          Quizzer = input.Quizzer }

let createEvents: CreateEvents =
    fun quizCode input currentQuizzerChangedOpt ->
        let currentChangedEvents =
            match currentQuizzerChangedOpt with
            | None -> []
            | Some event -> [ RemoveQuizzer.Event.CurrentQuizzerChanged event ]

        let quizzerRemovedEvent =
            RemoveQuizzer.Event.QuizzerNoLongerParticipating
                { Quiz = quizCode
                  Quizzer = input.Quizzer }

        [ yield quizzerRemovedEvent
          yield! currentChangedEvents ]


let removeQuizzer getQuiz (saveQuiz: SaveTeamQuizAsync) : RemoveQuizzer.Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.ofAsync
            let! validQuiz = validateRemoval validateQuiz quiz command.Data |> AsyncResult.ofResult

            let quiz, currentChangedEvent =
                removeQuizzerFromQuiz command.Data validQuiz []

            do! quiz |> Running |> saveQuiz |> AsyncResult.ofAsync

            return createEvents command.Quiz command.Data currentChangedEvent
        }

