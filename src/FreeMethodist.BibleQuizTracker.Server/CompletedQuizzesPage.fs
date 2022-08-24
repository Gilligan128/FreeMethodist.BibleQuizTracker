module FreeMethodist.BibleQuizTracker.Server.CompletedQuizzesPage

open Elmish

type ExternalMessage = | Error of string
                       | NoError

let update model message =
    model, Cmd.none, ExternalMessage.NoError

let render dispatch model =
    Bolero.Html.empty()
 