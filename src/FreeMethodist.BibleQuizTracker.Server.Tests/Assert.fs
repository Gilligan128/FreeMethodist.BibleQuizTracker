namespace global


[<RequireQualifiedAccess>]
module Assert =
    let onSuccess assertion result =
        match result with
        | Error error -> failwith $"{error}"
        | Ok result -> assertion result
