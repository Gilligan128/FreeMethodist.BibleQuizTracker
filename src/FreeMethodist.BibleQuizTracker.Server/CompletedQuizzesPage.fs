module FreeMethodist.BibleQuizTracker.Server.CompletedQuizzesPage

open Bolero
open Elmish
open Common_Page
open Bolero.Html
open CompletedQuizzesModel
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.CompletedQuizzesModel
open FreeMethodist.BibleQuizTracker.Server.RunningQuizPage
open FreeMethodist.BibleQuizTracker.Server.Workflow

type ExternalMessage =
    | Error of string
    | NoError

type Message =
    | Initialize of AsyncOperationStatus<unit, Result<QuizItem list, DbError>>

let init =
    {Quizzes = NotYetStarted}, () |> Started |> Initialize |> Cmd.ofMsg

let update getCompletedQuizzes model message =
    match message with
    | Initialize (Started _) ->
        let cmd = getCompletedQuizzes ()
                    |> AsyncResult.map (fun list -> list |> List.map (fun code -> { Code = code}))
                    |> Async.map (Initialize << Finished)
                    |> Cmd.OfAsync.result
        { model with Quizzes = InProgress }, cmd, ExternalMessage.NoError
    | Initialize (Finished result) -> {model with Quizzes = Resolved result}, Cmd.none, NoError
                   

let render dispatch model =
    match model.Quizzes with
    | NotYetStarted ->
        h1 {
            attr.``class`` "title"
            text "Quizzes not yet loaded"
        }
    | InProgress ->
        progress {
            attr.``class`` "progress is-primary"
            attr.max "100"
            "15%"
        }
    | Resolved (Result.Error error) -> p { text (error |> mapDbErrorToString) }
    | Resolved (Ok resolved) ->
        div {
            attr.``class`` "column"

            forEach resolved
            <| fun quiz ->
                div {
                    attr.``class`` "card"

                    div {
                        attr.``class`` "card-content"
                        quiz.Code
                    }
                }
        }
