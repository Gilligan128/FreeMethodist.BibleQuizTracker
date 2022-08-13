namespace FreeMethodist.BibleQuizTracker.Server
open Bolero
open Html

[<RequireQualifiedAccess>]
module Html =
    let capabilityButton dispatch colorOpt buttonText capOpt =
        button {
            attr.``class`` (
                "button "
                + match colorOpt with
                  | Some color -> $"is-{color}"
                  | None -> ""
            )

            attr.disabled (
                match capOpt with
                | Some _ -> null
                | None -> "disabled"
            )

            on.click (fun _ -> capOpt |> Option.iter dispatch)

            text buttonText
        }

    let capabilityLink colorOpt linkText capOpt =
        a {
            attr.``class`` (
                "button "
                + match colorOpt with
                  | Some color -> $"is-{color}"
                  | None -> ""
            )

            attr.disabled (
                match capOpt with
                | Some _ -> null
                | None -> "disabled"
            )

            attr.href (capOpt |> Option.defaultValue null)

            text linkText
        }

