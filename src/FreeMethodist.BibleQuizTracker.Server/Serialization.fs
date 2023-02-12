module FreeMethodist.BibleQuizTracker.Server.Serialization

open System.Text.Json
open System.Text.Json.Serialization

[<RequireQualifiedAccess>]
module FSharpSerializer =
    let fSharpOptions =
      let options = JsonSerializerOptions()
      do options.Converters.Add(JsonFSharpConverter(allowNullFields = true))
      options
