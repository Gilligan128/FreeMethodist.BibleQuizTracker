namespace global

module Option =

    // The apply function for Options
    let apply fOpt xOpt =
        match fOpt, xOpt with
        | Some f, Some x -> Some(f x)
        | _ -> None
  
    
    let transpose (optionOfResult: Option<Result<'a, 'b>>) : Result<Option<'a>,'b> =
            match optionOfResult with
            | Some (Ok a ) -> Ok (Some a)
            | Some (Error b) -> Error (b)
            | None -> Ok None