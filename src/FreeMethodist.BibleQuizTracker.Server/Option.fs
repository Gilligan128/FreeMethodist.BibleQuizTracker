module FreeMethodist.BibleQuizTracker.Server.Option

module Option =

    // The apply function for Options
    let apply fOpt xOpt =
        match fOpt, xOpt with
        | Some f, Some x -> Some(f x)
        | _ -> None
