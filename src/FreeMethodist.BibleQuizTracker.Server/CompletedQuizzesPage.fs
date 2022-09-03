module FreeMethodist.BibleQuizTracker.Server.CompletedQuizzesPage

open Bolero
open Elmish
open Common_Page
open Bolero.Html
open CompletedQuizzesModel

type ExternalMessage =
    | Error of string
    | NoError

let update model message =
    //match message with
    
    model, Cmd.none, ExternalMessage.NoError

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
