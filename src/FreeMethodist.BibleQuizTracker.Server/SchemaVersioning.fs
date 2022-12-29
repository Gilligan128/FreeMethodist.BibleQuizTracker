module FreeMethodist.BibleQuizTracker.Server.SchemaVersioning

open Microsoft.FSharp.Core

type SchemaVersion = int

type DeserializeJSON<'T> = SchemaVersion -> string -> 'T


