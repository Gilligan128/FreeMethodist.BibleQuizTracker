module FreeMethodist.BibleQuizTracker.Server.Index

open Bolero.Html
open Bolero.Server.Html

let page =
    doctypeHtml {
        head {
            meta { attr.charset "UTF-8" }

            meta {
                attr.name "viewport"
                attr.content "width=device-width, initial-scale=1.0"
            }

            title { "FMVB Quiz Tracker and Scoring" }
            ``base`` { attr.href "/" }

            link {
                attr.rel "stylesheet"
                attr.href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.4/css/bulma.min.css"
            }

            link {
                attr.rel "stylesheet"
                attr.href "css/index.css"
            }
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
                        attr.href "https://dev.fmquizzing.net/"

                        img {
                            attr.style "height:60px"
                            attr.src "https://dev.fmquizzing.net/resources/logos/FMBQ-logo.png"
                        }

                        "  Free Methodist Bible Quizzing"
                    }
                }
            }

            div {
                attr.id "main"
                rootComp<Main.MyApp>
            }

            boleroScript

            script {
                attr.defer ""
                attr.src "https://use.fontawesome.com/releases/v5.3.1/js/all.js"
            }
        }
    }
