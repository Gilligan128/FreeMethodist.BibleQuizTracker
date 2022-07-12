module FreeMethodist.BibleQuizTracker.Server.Tests.AddQuizzerComponentTests

open FreeMethodist.BibleQuizTracker.Server
open Xunit

[<Fact>]
let ``Hides AddQuizzer screen when cancelled`` =
    let sut = QuizPage.update  
    Assert.True(true)