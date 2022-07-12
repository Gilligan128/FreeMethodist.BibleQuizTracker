module FreeMethodist.BibleQuizTracker.Server.Tests.AddQuizzerComponentTests

open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Api
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain
open FreeMethodist.BibleQuizTracker.Server.QuizPage
open Xunit

[<Fact>]
let ``Given AddQuizzer is Active when Cancelled then AddQuizzer is Inert`` () =
    let connectToQuiz _ = Async.retn ()
    let publish _ _ = Async.retn ()

    let getQuiz _ =
        TeamQuiz.Unvalidated
            { Code = ""
              TeamOne = ""
              TeamTwo = "" }
    let saveQuiz _ = () 
    let sut =
        update connectToQuiz publish getQuiz saveQuiz 
    
    let initialModel = {emptyModel with AddQuizzer = Active ("",TeamPosition.TeamOne) }
    let resultingModel, cmd, externalMsg = sut (AddQuizzer Cancel) initialModel
    
    Assert.Equal(Inert, resultingModel.AddQuizzer)
