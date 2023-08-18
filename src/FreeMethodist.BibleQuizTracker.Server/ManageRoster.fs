namespace global

open Elmish
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server
open Bolero.Html


[<RequireQualifiedAccess>]
module ManageRoster =

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
          NewQuizzer: ManageRosterModelNewQuizzer option
          Capabilities: ManageRosterCapabilities }

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
        { Roster = roster
          NewQuizzer = None
          Capabilities = capabilities },
        Cmd.none

    let update message model =
        match message with
        | Message.Close -> None, Cmd.none, ExternalMessage.NoOp

    let private renderTeam removeCap dispatch team =
        div {
            attr.``class`` "columns"

            div {
                attr.``class`` "column"
                h3 { text team.Name }

                ul {
                    for quizzer in team.Quizzers do
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

    let render (model: Model option) dispatch =
        let removeCap quizzer =
            model
            |> Option.bind (fun model -> model.Capabilities.RemoveQuizzer)
            |> Option.map (fun remove -> fun () -> remove { Quizzer = quizzer })

        let renderTeam = renderTeam removeCap dispatch

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
                        | ModelRoster.Team (teamRoster1, teamRoster2) ->
                            renderTeams renderTeam (teamRoster1, teamRoster2)
                        | ModelRoster.Individual quizzers -> empty ()
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
