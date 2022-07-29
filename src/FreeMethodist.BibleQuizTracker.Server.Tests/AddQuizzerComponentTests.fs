module FreeMethodist.BibleQuizTracker.Server.Tests.AddQuizzerComponentTests

open Elmish
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.QuizPage
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit

let publishQuiz _ _ _ = Async.retn ()

let getQuiz _ =
    Quiz.Unvalidated
        { Code = ""
          CompetitionStyle = Team { TeamOneName = ""; TeamTwoName = "" } }

let getQuizAsync code = getQuiz code |> AsyncResult.retn

let saveQuiz _ = ()
let saveQuizAsync _ = AsyncResult.retn ()

let sut =
    update (fun _ _ -> Async.retn ()) (fun _ -> ()) publishQuiz getQuizAsync saveQuizAsync

let mapToLoaded model =
    match model with
    | NotYetLoaded _ -> failwith "not yet loaded"
    | Loading _ -> failwith "loading"
    | Loaded loadedModel -> loadedModel

[<Fact>]
let ``When Cancelled then AddQuizzer is Inert`` () =

    let initialModel =
        Loaded { emptyModel with AddQuizzer = AddQuizzerModel.Active("", TeamOne) }

    let resultingModel, cmd, externalMsg =
        sut (AddQuizzer Cancel) initialModel
    let resultingModel = mapToLoaded resultingModel

    Assert.Equal(Inert, resultingModel.AddQuizzer)
    Assert.Equal<Cmd<Message>>(Cmd.none, cmd)
    Assert.Equal(None, externalMsg)

[<Fact>]
let ``Given AddQuizzer is Inert when Started then AddQuizzer is Active`` () =

    let initialModel =
        Loaded { emptyModel with AddQuizzer = Inert }

    let resultingModel, cmd, externalMsg =
        sut (AddQuizzer Start) initialModel
    let resultingModel = mapToLoaded resultingModel
    
    Assert.Equal(AddQuizzerModel.Active("", TeamOne), resultingModel.AddQuizzer)
    Assert.Equal<Cmd<Message>>(Cmd.none, cmd)
    Assert.Equal(None, externalMsg)

[<Fact>]
let ``Given AddQuizzer is Active when Started then AddQuizzer is Active with original data`` () =
    let activeState =
        AddQuizzerModel.Active("test", TeamTwo)

    let initialModel =
        Loaded { emptyModel with AddQuizzer = activeState }

    let resultingModel, cmd, externalMsg =
        sut (AddQuizzer Start) initialModel
    let resultingModel = mapToLoaded resultingModel
    
    Assert.Equal(activeState, resultingModel.AddQuizzer)
    Assert.Equal<Cmd<Message>>(Cmd.none, cmd)
    Assert.Equal(None, externalMsg)
