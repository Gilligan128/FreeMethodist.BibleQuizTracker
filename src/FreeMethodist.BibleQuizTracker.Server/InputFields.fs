module FreeMethodist.BibleQuizTracker.Server.InputFields

open System.Text.RegularExpressions
open Bolero
open Bolero.Html

let  labeledField fieldLabel field changeAction =
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

let  labeledSelect fieldLabel changeAction options field =
    div {
        attr.``class`` "field"

        div {
            attr.``class`` "control"

            label {
                attr.``class`` "label"

                $"{fieldLabel}:"
            }

            div {
                attr.``class`` "select"

                select {
                    bind.change.string (string field) (changeAction)
                   
                    forEach options
                    <| fun  opt -> 
                        let optString = string opt
                        option {
                            attr.value (optString)
                            text (Regex.Replace(optString, "(\\B[A-Z])", " $1"))
                        }
                }
            }
        }
    }
