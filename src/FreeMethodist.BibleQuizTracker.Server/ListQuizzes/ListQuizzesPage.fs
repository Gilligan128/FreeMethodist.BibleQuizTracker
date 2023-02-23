module FreeMethodist.BibleQuizTracker.Server.ListQuizzesPage

open Bolero
open Elmish
open Common_Page
open Bolero.Html
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Routing
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Common.Pipeline

type ExternalMessage =
    | Error of string
    | NoError

type Message =
    | Initialize of AsyncOperationStatus<unit, Result<ListQuizItem list, DbError>>

let init =
    {
        StateFilter = QuizStatusFilter.All 
        Quizzes = NotYetStarted
    }, () |> Started |> Initialize |> Cmd.ofMsg

let update getQuizzes model message : ListQuizModel*Cmd<Message>*ExternalMessage=
    match message with
    | Initialize (Started _) ->
        let cmd = getQuizzes model.StateFilter
                    |> Async.map (Initialize << Finished)
                    |> Cmd.OfAsync.result
        { model with Quizzes = InProgress }, cmd, ExternalMessage.NoError
    | Initialize (Finished result) -> {model with Quizzes = Resolved result}, Cmd.none, NoError
                   

let render link dispatch model =
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
        concat {
            h1 {
                attr.``class`` "title"
                "Quizzes"
            }
            div {
                attr.``class`` "column"

                forEach resolved
                <| fun quiz ->
                    div {
                        attr.``class`` "card"

                        div {
                            attr.``class`` "card-content"
                            
                            a {
                                attr.href (link (Page.QuizDetails (quiz.Code, Router.noModel) ))
                                quiz.Code
                               }
                        }
                    }
            }
        }
