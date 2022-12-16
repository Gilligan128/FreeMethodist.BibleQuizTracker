module FreeMethodist.BibleQuizTracker.Server.Tests.AddQuizzerComponentTests

open Elmish
open FreeMethodist.BibleQuizTracker.Server.Capabilities
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunningQuizPage
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open Xunit

let publishQuiz _ _ _ = Async.retn ()

let getQuizAsync code =
    RunningQuiz.newTeamQuiz
    |> Running
    |> AsyncResult.retn

let tryGetQuizAsync code =
    RunningQuiz.newTeamQuiz
    |> Running
    |> Some
    |> AsyncResult.retn

let capabilitiesForQuizProvider: RunQuizCapabilityForQuizProvider =
    { AddQuizzer = fun _ _-> None
      RemoveQuizzer = fun _ _-> None
      AnswerCorrectly = fun _ _ -> None
      AnswerIncorrectly = fun _ _ -> None
      FailAppeal = fun _ _ -> None
      ClearAppeal = fun _ _ -> None
      SelectQuizzer = fun _ _ -> None
      ChangeCurrentQuestion = fun _ _-> None
      CompleteQuiz = fun _ _ -> None
      ReopenQuiz = fun _ _ -> None
      Prejump = fun _ _ -> None }

let sut =
    update (fun _ _ -> ignore) publishQuiz getQuizAsync tryGetQuizAsync ignore  capabilitiesForQuizProvider

let mapToLoaded model =
    match model.Info with
    | NotYetStarted -> failwith "not yet loaded"
    | InProgress -> failwith "loading"
    | Resolved loadedModel -> loadedModel

[<Fact>]
let ``When Cancelled then AddQuizzer is Inert`` () =

    let initialModel =
        { Code = ""
          User = Scorekeeper
          Info = Resolved { emptyModel with AddQuizzer = AddQuizzerModel.Active("", TeamOne) } }

    let resultingModel, cmd, externalMsg =
        sut (AddQuizzer Cancel) initialModel

    let resultingModel =
        mapToLoaded resultingModel

    Assert.Equal(Inert, resultingModel.AddQuizzer)
    Assert.Equal<Cmd<Message>>(Cmd.none, cmd)
    Assert.Equal(NoMessage, externalMsg)

[<Fact>]
let ``Given AddQuizzer is Inert when Started then AddQuizzer is Active`` () =

    let initialModel =
       {Code =""; User = Scorekeeper; Info = Resolved { emptyModel with AddQuizzer = Inert }}

    let resultingModel, cmd, externalMsg =
        sut (AddQuizzer Start) initialModel

    let resultingModel =
        mapToLoaded resultingModel

    Assert.Equal(AddQuizzerModel.Active("", TeamOne), resultingModel.AddQuizzer)
    Assert.Equal<Cmd<Message>>(Cmd.none, cmd)
    Assert.Equal(NoMessage, externalMsg)

[<Fact>]
let ``Given AddQuizzer is Active when Started then AddQuizzer is Active with original data`` () =
    let activeState =
        AddQuizzerModel.Active("test", TeamTwo)

    let initialModel =
        {Code =""; User = Scorekeeper; Info = Resolved { emptyModel with AddQuizzer = activeState }}

    let resultingModel, cmd, externalMsg =
        sut (AddQuizzer Start) initialModel

    let resultingModel =
        mapToLoaded resultingModel

    Assert.Equal(activeState, resultingModel.AddQuizzer)
    Assert.Equal<Cmd<Message>>(Cmd.none, cmd)
    Assert.Equal(NoMessage, externalMsg)
