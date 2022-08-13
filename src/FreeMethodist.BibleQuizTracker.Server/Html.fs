namespace FreeMethodist.BibleQuizTracker.Server
open Bolero
open Html

[<RequireQualifiedAccess>]
module Html =
    
    let disabledIfNone opt =
         attr.disabled (
                match opt with
                | Some _ -> null
                | None -> "disabled"
            )
    
    let capabilityButton dispatch colorOpt buttonText capOpt =
        button {
            attr.``class`` (
                "button "
                + match colorOpt with
                  | Some color -> $"is-{color}"
                  | None -> ""
            )

            capOpt |> disabledIfNone 

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

            capOpt |> disabledIfNone

            attr.href (capOpt |> Option.defaultValue null)

            text linkText
        }

    