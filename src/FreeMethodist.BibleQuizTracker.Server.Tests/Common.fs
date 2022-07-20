module FreeMethodist.BibleQuizTracker.Server.Tests.Common

open Xunit

let assertSuccess result assertion =
    match result with
    | Error errorValue -> Assert.True(false, $"Received {errorValue}")
    | Ok success -> assertion success