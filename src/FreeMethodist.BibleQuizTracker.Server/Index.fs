module FreeMethodist.BibleQuizTracker.Server.Index

open Bolero
open Bolero.Html
open Bolero.Server.Html
open FreeMethodist.BibleQuizTracker

let page = doctypeHtml {
    head {
        meta { attr.charset "UTF-8" }
        meta { attr.name "viewport"; attr.content "width=device-width, initial-scale=1.0" }
        title { "FMVB Quiz Tracker and Scoring" }
        ``base`` { attr.href "/" }
        link { attr.rel "stylesheet"; attr.href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css" }
        link { attr.rel "stylesheet"; attr.href "css/index.css" }
    }
    body {
        nav {
            attr.``class`` "navbar is-dark"
            "role" => "navigation"
            attr.aria "label" "main navigation"
            div {
                attr.``class`` "navbar-brand"
                a {
                    attr.``class`` "navbar-item has-text-weight-bold is-size-5"
                    attr.href "https://fsbolero.io"
                    img { attr.style "height:40px"; attr.src "https://github.com/fsbolero/website/raw/master/src/Website/img/wasm-fsharp.png" }
                    "  Bolero"
                }
            }
        }
        div { attr.id "main"; rootComp<Main.MyApp> }
        boleroScript
        script {
            attr.defer ""
            attr.src "https://use.fontawesome.com/releases/v5.3.1/js/all.js"
        }
    }
}
