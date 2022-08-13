module FreeMethodist.BibleQuizTracker.Server.Tests.AddQuizzerComponentTests

open Elmish
open FreeMethodist.BibleQuizTracker.Server.Capabilities.Capabilities
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunningQuizPage
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Capabilities
open Xunit

let publishQuiz _ _ _ = Async.retn ()

let getQuizAsync code = RunningTeamQuiz.identity |> Running |> AsyncResult.retn
let tryGetQuizAsync code = RunningTeamQuiz.identity |> Running |> Some |> AsyncResult.retn


let capabilitiesProvider : RunQuizCapabilityProvider= {
    AddQuizzer = fun _ -> Some (fun _ -> AsyncResult.ofSuccess Unchecked.defaultof<QuizzerParticipating>)
    RemoveQuizzer = fun _ -> Some (fun _ -> AsyncResult.ofSuccess [])
    AnswerCorrectly = fun _ _ -> Some (fun _ -> AsyncResult.ofSuccess [])
    AnswerIncorrectly = fun _ _ -> Some (fun _ -> AsyncResult.ofSuccess [])
    FailAppeal = fun _ _ -> Some (fun _ -> AsyncResult.ofSuccess [])
    ClearAppeal = fun _ _ -> Some (fun _ -> AsyncResult.ofSuccess [])
    SelectQuizzer = fun _ -> Some (fun _ -> AsyncResult.ofSuccess Unchecked.defaultof<CurrentQuizzerChanged>)
    ChangeCurrentQuestion = fun _ -> Some (fun _ -> AsyncResult.ofSuccess Unchecked.defaultof<CurrentQuestionChanged>)
    CompleteQuiz = fun _ -> None
    ReopenQuiz = fun _ -> None
}

let sut =
    update (fun _  _ -> ignore)  publishQuiz getQuizAsync tryGetQuizAsync ignore capabilitiesProvider 

let mapToLoaded model =
    match model with
    | NotYetStarted _ -> failwith "not yet loaded"
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
    Assert.Equal(NoMessage, externalMsg)

[<Fact>]
let ``Given AddQuizzer is Inert when Started then AddQuizzer is Active`` () =

    let initialModel =
        Loaded { emptyModel with AddQuizzer = Inert }

    let resultingModel, cmd, externalMsg =
        sut (AddQuizzer Start) initialModel
    let resultingModel = mapToLoaded resultingModel
    
    Assert.Equal(AddQuizzerModel.Active("", TeamOne), resultingModel.AddQuizzer)
    Assert.Equal<Cmd<Message>>(Cmd.none, cmd)
    Assert.Equal(NoMessage, externalMsg)

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
    Assert.Equal(NoMessage, externalMsg)
