module FreeMethodist.BibleQuizTracker.Server.Tests.AddQuizzerComponentTests

open Elmish
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Workflow
open FreeMethodist.BibleQuizTracker.Server.Pipeline
open FreeMethodist.BibleQuizTracker.Server.QuizPage
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit

let connectToQuiz _ _ = Async.retn ()
let publishQuiz _ _ _ = Async.retn ()

let getQuiz _ =
    Quiz.Unvalidated
        { Code = ""
          TeamOne = ""
          TeamTwo = "" }
let getQuizAsync code = getQuiz code |> Async.retn

let saveQuiz _ = ()
let saveQuizAsync _= Async.retn ()

let sut =
    update connectToQuiz publishQuiz getQuizAsync saveQuizAsync

[<Fact>]
let ``When Cancelled then AddQuizzer is Inert`` () =

    let initialModel =
        { emptyModel with AddQuizzer = AddQuizzerModel.Active("", TeamOne) }

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

    Assert.Equal(AddQuizzerModel.Active("", TeamOne), resultingModel.AddQuizzer)
    Assert.Equal<Cmd<Message>>(Cmd.none, cmd)
    Assert.Equal(None, externalMsg)

[<Fact>]
let ``Given AddQuizzer is Active when Started then AddQuizzer is Active with original data`` () =
    let activeState =
        AddQuizzerModel.Active("test", TeamTwo)

    let initialModel =
        { emptyModel with AddQuizzer = activeState }

    let resultingModel, cmd, externalMsg =
        sut (AddQuizzer Start) initialModel

    Assert.Equal(activeState, resultingModel.AddQuizzer)
    Assert.Equal<Cmd<Message>>(Cmd.none, cmd)
    Assert.Equal(None, externalMsg)
