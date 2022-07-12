module FreeMethodist.BibleQuizTracker.Server.Tests.AddQuizzerComponentTests

open Elmish
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Api
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain
open FreeMethodist.BibleQuizTracker.Server.QuizPage
open Xunit

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

[<Fact>]
let ``When Cancelled then AddQuizzer is Inert`` () =

    let initialModel =
        { emptyModel with AddQuizzer = Active("", TeamPosition.TeamOne) }

    let resultingModel, cmd, externalMsg =
        sut (AddQuizzer Cancel) initialModel

    Assert.Equal(Inert, resultingModel.AddQuizzer)
    Assert.Equal<Cmd<Message>>(Cmd.none, cmd)
    Assert.Equal(None, externalMsg)

[<Fact>]
let ``Given AddQuizzer is Inert when Started then AddQuizzer is Active`` () =
    
    let initialModel =
        { emptyModel with AddQuizzer = Inert }
    let resultingModel, cmd, externalMsg =
        sut (AddQuizzer Start) initialModel

    Assert.Equal(Active ("", TeamOne), resultingModel.AddQuizzer)
    Assert.Equal<Cmd<Message>>(Cmd.none, cmd)
    Assert.Equal(None, externalMsg)
    
[<Fact>]
let ``Given AddQuizzer is Active when Started then AddQuizzer is Active with original data`` () =
    let activeState = Active ("test", TeamTwo)
    let initialModel =
        { emptyModel with AddQuizzer = activeState }
    let resultingModel, cmd, externalMsg =
        sut (AddQuizzer Start) initialModel

    Assert.Equal(activeState, resultingModel.AddQuizzer)
    Assert.Equal<Cmd<Message>>(Cmd.none, cmd)
    Assert.Equal(None, externalMsg)