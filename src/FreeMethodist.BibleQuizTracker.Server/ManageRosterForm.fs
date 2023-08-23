namespace global

open Elmish
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server
open Bolero.Html
open Bolero


[<RequireQualifiedAccess>]
module ManageRosterForm =

    type ManageRosterModelNewQuizzer =
        | Team of string * TeamPosition
        | Individual of string

    type TeamRoster =
        { Name: string; Quizzers: Quizzer list }

    type ModelRoster =
        | Team of TeamRoster * TeamRoster
        | Individual of Quizzer list

    type ManageRosterCapabilities =
        { RemoveQuizzer: (RemoveQuizzer.Data -> AsyncResult<RemoveQuizzer.Event list, RemoveQuizzer.Error>) option
          AddQuizzer: (AddQuizzer.Data -> AsyncResult<QuizzerParticipating, AddQuizzer.Error>) option }

    type Model =
        { Roster: ModelRoster
          NewQuizzer: ManageRosterModelNewQuizzer option }

    type MessageNewQuizzer =
        | Team of string * TeamPosition
        | Individual of string

    type Message =
        | Close
        | NewQuizzer of MessageNewQuizzer
        | AddQuizzer of AsyncOperationStatus<AddQuizzer.Data, Result<QuizzerParticipating, AddQuizzer.Error>>
        | RemoveQuizzer of
            AsyncOperationStatus<unit -> AsyncResult<RemoveQuizzer.Event list, RemoveQuizzer.Error>, Result<RemoveQuizzer.Event list, RemoveQuizzer.Error>>
        | SetName of string
        | CancelNewQuizzer

    type ExternalMessage =
        | Error of string
        | WorkflowSuccess of RunQuizEvent list
        | NoOp

    let init (capabilities: ManageRosterCapabilities) roster =
        { Roster = roster; NewQuizzer = None }, Cmd.none

    let private mapToAsyncOperationCmd asyncOperation resultAsync =
        resultAsync
        |> Async.map (fun result -> asyncOperation (Finished result))
        |> Cmd.OfAsync.result


    let update message model =
        match message with
        | Message.Close -> None, Cmd.none, ExternalMessage.NoOp
        | Message.RemoveQuizzer(Started cap) ->
            let mapEvent event =
                match event with
                | RemoveQuizzer.Event.CurrentQuizzerChanged e -> RunQuizEvent.CurrentQuizzerChanged e
                | RemoveQuizzer.Event.QuizzerNoLongerParticipating e -> RunQuizEvent.QuizzerNoLongerParticipating e

            let cmd = cap () |> mapToAsyncOperationCmd Message.RemoveQuizzer
            Some model, cmd, ExternalMessage.NoOp
        | Message.RemoveQuizzer(Finished result) ->
            match result with
            | Ok events ->
                let events =
                    events
                    |> List.map (fun event ->
                        match event with
                        | RemoveQuizzer.CurrentQuizzerChanged e -> RunQuizEvent.CurrentQuizzerChanged e
                        | RemoveQuizzer.QuizzerNoLongerParticipating e -> RunQuizEvent.QuizzerNoLongerParticipating e)

                Some model, Cmd.none, ExternalMessage.WorkflowSuccess events
            | Result.Error error ->
                let errorText =
                    match error with
                    | RemoveQuizzer.DbError(error) -> error |> mapDbErrorToString
                    | RemoveQuizzer.QuizStateError error -> "Quiz is in the wrong state"
                    | RemoveQuizzer.QuizzerNotParticipating error -> $"{error} is not participating in the quiz"

                Some model, Cmd.none, ExternalMessage.Error errorText

    let private renderQuizzers removeCap dispatch quizzers =
        ul {
            for quizzer in quizzers do
                let removeCap = removeCap |> fun cap -> cap quizzer

                li {
                    text quizzer

                    button {
                        attr.``class`` "button is-info is-light"

                        removeCap |> Html.disabledIfNone

                        on.click (fun _ ->
                            removeCap
                            |> Option.iter (fun cap -> cap |> Started |> RemoveQuizzer |> dispatch))

                        span {
                            attr.``class`` "icon"
                            i { attr.``class`` "fas fa-times-circle" }
                        }
                    }
                }
        }

    let private renderTeam (renderQuizzers: Quizzer list -> Node) team =
        div {
            attr.``class`` "columns"

            div {
                attr.``class`` "column"
                h3 { text team.Name }

                renderQuizzers team.Quizzers
            }
        }

    let private renderTeams (renderTeam: TeamRoster -> Bolero.Node) (teamRoster1, teamRoster2) =
        div {
            attr.``class`` "columns"

            div {
                attr.``class`` "column"

                renderTeam teamRoster1
            }

            div {
                attr.``class`` "column"

                renderTeam teamRoster2
            }
        }

    let render capabilities (model: Model option) dispatch =
        let removeCap quizzer =
            model
            |> Option.bind (fun model -> capabilities.RemoveQuizzer)
            |> Option.map (fun remove -> fun () -> remove { Quizzer = quizzer })

        let renderQuizzers = renderQuizzers removeCap dispatch
        let renderTeam = renderTeam renderQuizzers

        div {
            attr.``class`` (
                match model with
                | Some _ -> "modal is-active"
                | _ -> "modal"
            )

            attr.id "manage-roster-modal"
            div { attr.``class`` "modal-background" }

            div {
                attr.``class`` "modal-card"

                header {
                    attr.``class`` "modal-card-head"

                    p {
                        attr.``class`` "modal-card-title"
                        "Manage Roster"
                    }

                    button {
                        attr.``class`` "delete"
                        on.click (fun _ -> dispatch (Message.Close))
                    }
                }

                section {
                    attr.``class`` "modal-card-body"

                    match model with
                    | Some model ->
                        match model.Roster with
                        | ModelRoster.Team(teamRoster1, teamRoster2) ->
                            renderTeams renderTeam (teamRoster1, teamRoster2)
                        | ModelRoster.Individual quizzers ->
                            renderQuizzers quizzers

                            match model.NewQuizzer with
                            | None ->
                                button {
                                    attr.``class`` "button is-info is-light"

                                    on.click (fun _ -> dispatch (Message.NewQuizzer(Individual "")))

                                    span {
                                        attr.``class`` "icon"
                                        i { attr.``class`` "fas fa-solid fa-plus" }
                                    }
                                }
                            | Some _-> empty()
                    | None -> empty ()
                //Add team rosters and buttons to "newquizzer" state
                //NewQuizzer state - no new is a button to add a new quizzer for either team, or in individuals for the next quizzer.
                //NewQuizzer state - adding is a field for the quizzer name, a button to save, and a button to cancel.
                }

                footer {
                    attr.``class`` "modal-card-foot"

                    div {
                        button {
                            attr.``class`` "button"
                            on.click (fun _ -> dispatch (Message.Close))
                            text "Close"
                        }
                    }
                }
            }
        }
