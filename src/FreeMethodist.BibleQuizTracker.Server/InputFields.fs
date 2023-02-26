module FreeMethodist.BibleQuizTracker.Server.InputFields

open System.Text.RegularExpressions
open Bolero
open Bolero.Html

let bulmaInput changeAction field =
    input {
        attr.``class`` "input"

        bind.input.string field (changeAction)
    }

let bulmaLabel fieldLabel =
    label {
        attr.``class`` "label"

        $"{fieldLabel}"
    }

let bulmaControl (inner: Node) =
    div {
        attr.``class`` "control"

        inner
    }

let labeledField fieldLabel field changeAction =
    div {
        attr.``class`` "field"

        div {
            attr.``class`` "control"

            label {
                attr.``class`` "label"

                $"{fieldLabel}:"
            }

            input {
                attr.``class`` "input"

                bind.input.string field (changeAction)
            }
        }
    }

let bulmaSelect changeAction options field =
    div {
        attr.``class`` "select"
        select {
            bind.change.string (field) (changeAction)

            forEach options
            <| fun (value, display) ->
                option {
                    attr.value value

                    text display
                }
        }
    }

let labeledSelectControl fieldLabel changeAction options field =
    div {
        attr.``class`` "control"

        label {
            attr.``class`` "label"

            $"{fieldLabel}:"
        }

        div {
            attr.``class`` "select"

            bulmaSelect changeAction options field
        }
    }
