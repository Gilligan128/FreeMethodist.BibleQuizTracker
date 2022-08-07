module FreeMethodist.BibleQuizTracker.Server.QuizDetailsPage

open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView
open FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView.ItemizedScore
open FreeMethodist.BibleQuizTracker.Server.Routing
open FreeMethodist.BibleQuizTracker.Server.Workflow


type Message = Initialize of AsyncOperationStatus<unit, Result<Quiz option, DbError>>

let init quizCode =
    { Code = quizCode
      Details = NotYetStarted },
    Cmd.ofMsg (Initialize(Started()))

let subOfFunc arg (func: 'a -> unit) : Sub<Message> = fun _ -> func arg

let update tryGetQuiz navigate (model : QuizDetailsModel) message =
    let navigateHomeCmd =
        navigate |> subOfFunc Page.Home |> Cmd.ofSub

    match message with
    | Initialize (Started _) ->
        let cmd =
            model.Code
            |> tryGetQuiz
            |> Async.map Finished
            |> Async.map Initialize
            |> Cmd.OfAsync.result

        { model with Details = InProgress }, cmd
    | Initialize (Finished (Ok (Some quiz))) -> { model with Details = InProgress }, Cmd.none
    | Initialize (Finished (Ok None)) -> { model with Details = NotYetStarted }, navigateHomeCmd
    | Initialize (Finished (Error error)) -> model, navigateHomeCmd


let render dispatch (model: QuizDetailsModel) : Node =
    match model.Details with
    | NotYetStarted ->
        p {
            attr.``class`` "title"
            $"Quiz {model.Code} is not yet loaded"
        }
    | InProgress ->
        div {
            progress {
                attr.``class`` "progress is-primary"
                attr.max "100"
                "15%"
            }
        }
    | Resolved loadedModel ->
        concat {
            render loadedModel.ItemizedScore dispatch

        }
