﻿module FreeMethodist.BibleQuizTracker.Server.QuizDetailsPage

open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Capabilities
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView
open FreeMethodist.BibleQuizTracker.Server.Routing
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows

//leaky abstraction here where we render links at the same abstraction level as our "biz logic functions"
//I think we want to transform the functions into links as necessary from strong types.
type QuizControlCapabilityProvider =
    { CompleteQuiz: Quiz -> CompleteQuizCap option
      ReopenQuiz: Quiz -> ReopenQuizCap option
      RunQuiz: Quiz -> Link option
      Spectate: Quiz -> Link option
      LiveScore: Quiz -> Link option }

type QuizDetailsMessage =
    | Initialize of AsyncOperationStatus<unit, Result<Quiz option, DbError>>
    | CompleteQuiz of AsyncOperationStatus<CompleteQuizCap, Result<Quiz, WorkflowError<CompleteQuiz.Error>>>
    | ReopenQuiz of AsyncOperationStatus<ReopenQuizCap, Result<Quiz, WorkflowError<ReopenQuiz.Error>>>

type ExternalMessage =
    | ErrorMessage of string
    | NoMessage

let init quizCode =
    { Code = quizCode
      Details = Deferred.NotYetStarted },
    Cmd.ofMsg (Initialize(Started()))

let subOfFunc arg (func: 'a -> unit) : Sub<QuizDetailsMessage> = fun _ -> func arg

let loadRunningQuizzer (quizzer : QuizzerState) =
   quizzer |> (fun q -> q.Name)

let loadRunningTeam (team: QuizTeamState) : ItemizedTeam =
    { Name = team.Name
      Quizzers = team.Quizzers |> List.map loadRunningQuizzer }

let private loadRunningQuiz quiz =
    { NumberOfQuestions = quiz.Questions |> Map.keys |> Seq.max
      QuestionsWithEvents =
        quiz.Questions
        |> Score.createScoreModel
      CompetitionStyle =
          match quiz.CompetitionStyle with
          | RunningCompetitionStyle.Team(teamOne, teamTwo) -> ItemizedCompetitionStyle.Team (loadRunningTeam teamOne, loadRunningTeam teamTwo)
          | RunningCompetitionStyle.Individuals quizzerStates -> quizzerStates |> List.map loadRunningQuizzer |> ItemizedCompetitionStyle.Individual
    }

let private loadCompletedTeam (team: CompletedTeam) : ItemizedTeam =
    { Name = team.Name
      Quizzers = team.Quizzers |> List.map (fun q -> q.Name) }

let private loadCompletedQuiz (quiz: CompletedQuiz) =
    { NumberOfQuestions =
        quiz.CompletedQuestions.Length
        |> PositiveNumber.numberOrOne
      QuestionsWithEvents =
        quiz.CompletedQuestions
        |> List.indexed
        |> List.collect (fun (i, v) ->
            Score.createScoreModelForQuestion
                (i + 1 |> PositiveNumber.numberOrOne)
                { AnswerState = (Complete v.AnswerState); FailedAppeals = v.FailedAppeals; Prejumps = [] })
      CompetitionStyle =
        match quiz.CompetitionStyle with
        | CompletedCompetitionStyle.Team (teamOne, teamTwo) ->
            ItemizedCompetitionStyle.Team(teamOne |> loadCompletedTeam, teamTwo |> loadCompletedTeam)
        | CompletedCompetitionStyle.Individual completedQuizzers ->
            completedQuizzers
            |> List.map (fun q -> q.Name)
            |> ItemizedCompetitionStyle.Individual }

let private availableCapabilities (provider: QuizControlCapabilityProvider) quiz =
    let completeQuiz =
        provider.CompleteQuiz quiz

    let reopenQuiz = provider.ReopenQuiz quiz
    let spectateQuiz = provider.Spectate quiz
    let liveScoreQuiz = provider.LiveScore quiz
    let runQuiz = provider.RunQuiz quiz
    completeQuiz, reopenQuiz, runQuiz, spectateQuiz, liveScoreQuiz


let private reloadModel capabilityProvider model quiz =
    let completeQuiz, reopenQuiz, runQuiz, spectateCap, liveScoreCap =
        availableCapabilities capabilityProvider quiz

    let capabilities: QuizControlCapabilities =
        { CompleteQuiz = completeQuiz
          ReopenQuiz = reopenQuiz
          Run = runQuiz
          Spectate = spectateCap
          LiveScore = liveScoreCap }

    let details =
        match quiz with
        | Quiz.Running running ->
            { State = nameof Running
              Capabilities = capabilities
              ItemizedScore = running |> loadRunningQuiz }
        | Quiz.Completed completedQuiz ->
            { State = nameof Completed
              Capabilities = capabilities
              ItemizedScore = completedQuiz |> loadCompletedQuiz }
        | Quiz.Official _ ->
            { State = nameof Official
              Capabilities = capabilities
              ItemizedScore = Unchecked.defaultof<ItemizedScoreModel> }
        |> Resolved

    { model with Details = details }

let private startWorkflow (getQuiz: GetQuiz) model getCode finishedMessage workflowCap =
    workflowCap
    |> AsyncResult.mapError WorkflowError.Workflow
    |> AsyncResult.bind (fun _ ->
        model
        |> getCode
        |> getQuiz
        |> (AsyncResult.mapError WorkflowError.DbError))
    |> Async.map Finished
    |> Async.map finishedMessage
    |> fun async -> model, Cmd.OfAsync.result async, NoMessage


let private finishWorkflow reloadModel  mapWorkflowErrors model result =
    match result with
    | Ok quiz -> quiz |> reloadModel model, Cmd.none, NoMessage
    | Result.Error error ->
        model,
        Cmd.none,
        (match error with
         | WorkflowError.Workflow er -> er |> mapWorkflowErrors
         | WorkflowError.DbError dbError -> dbError |> mapDbErrorToString)
        |> ErrorMessage

let update
    tryGetQuiz
    navigate
    (capabilityProvider: QuizControlCapabilityProvider)
    getQuiz
    (model: QuizDetailsModel)
    message
    =
    let navigateHomeCmd =
        navigate |> subOfFunc Page.Home |> Cmd.ofSub
    
    let reloadModel = reloadModel capabilityProvider
    
    let startWorkflow finishedMessage cap =
        startWorkflow getQuiz model (fun model -> model.Code) finishedMessage cap

    let finishWorkflow mapWorkflowErrors result =
        finishWorkflow reloadModel mapWorkflowErrors model result

    match message with
    | Initialize (Started _) ->
        let cmd =
            model.Code
            |> tryGetQuiz
            |> Async.map Finished
            |> Async.map Initialize
            |> Cmd.OfAsync.result

        { model with Details = InProgress }, cmd, NoMessage
    | Initialize (Finished (Ok (Some quiz))) -> reloadModel model quiz, Cmd.none, NoMessage
    | Initialize (Finished (Ok None)) ->
        { model with Details = Deferred.NotYetStarted }, navigateHomeCmd, $"Quiz {model.Code} not found" |> ErrorMessage
    | Initialize (Finished (Result.Error error)) -> model, navigateHomeCmd, error |> mapDbErrorToString |> ErrorMessage
    | CompleteQuiz (Started cap) ->
        cap ()
        |> startWorkflow QuizDetailsMessage.CompleteQuiz
    | CompleteQuiz (Finished result) ->
        let mapError error =
            match error with
            | CompleteQuiz.Error.DbError dbError -> dbError |> mapDbErrorToString
            | CompleteQuiz.QuizState _ -> "Wrong quiz state"

        result |> finishWorkflow mapError
    | ReopenQuiz (Started cap) ->
        cap ()
        |> startWorkflow QuizDetailsMessage.ReopenQuiz
    | ReopenQuiz (Finished result) ->
        let mapError error =
            match error with
            | ReopenQuiz.DbError dbError -> dbError |> mapDbErrorToString
            | ReopenQuiz.QuizState _ -> "Wrong quiz state"

        result |> finishWorkflow mapError

let private dispatchedMessage dispatch cap = fun () -> dispatch cap

let render (dispatch: Dispatch<QuizDetailsMessage>) (model: QuizDetailsModel) : Node =
    match model.Details with
    | Deferred.NotYetStarted ->
        p {
            attr.``class`` "title"
            $"Quiz {model.Code} is not yet loaded"
        }
    | InProgress ->
        div {
            progress {
                attr.``class`` "progress is-primary"
                attr.max "100"
                "15%"
            }
        }
    | Resolved loadedModel ->
        div {
            attr.``class`` "column is-two-thirds"

            div {
                attr.``class`` "columns "

                div {
                    attr.``class`` "column "

                    p {
                        attr.``class`` "title"
                        $"Quiz {model.Code} Details"
                    }

                }

                div {
                    attr.``class`` "column tags has-addons"
                    span {
                        attr.``class`` "tag is-large"
                        "Status"
                    }
                    span {
                        attr.``class`` "tag is-success is-large"
                        loadedModel.State
                    }
                }
            }

            div {
                attr.``class`` "buttons"

                loadedModel.Capabilities.CompleteQuiz
                |> Option.map (Started >> QuizDetailsMessage.CompleteQuiz)
                |> Html.capabilityButton dispatch (Some "primary") "Complete"

                loadedModel.Capabilities.ReopenQuiz
                |> Option.map (Started >> QuizDetailsMessage.ReopenQuiz)
                |> Html.capabilityButton dispatch (Some "primary") "Reopen"

                loadedModel.Capabilities.Run
                |> Html.capabilityLink (Some "link") "Run"

                loadedModel.Capabilities.Spectate
                |> Html.capabilityLink (Some "info") (nameof loadedModel.Capabilities.Spectate)

                loadedModel.Capabilities.LiveScore
                |> Html.capabilityLink (Some "info") "Live Score"

            }

            ItemizedScore.render loadedModel.ItemizedScore dispatch

        }
