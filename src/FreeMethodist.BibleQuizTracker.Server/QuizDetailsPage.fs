module FreeMethodist.BibleQuizTracker.Server.QuizDetailsPage

open Bolero
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView
open FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView.ItemizedScore
open FreeMethodist.BibleQuizTracker.Server.Workflow



let render dispatch model : Node =
    Html.empty()