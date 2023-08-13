namespace global

open Elmish
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
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
            AsyncOperationStatus<RemoveQuizzer.Data, Result<RemoveQuizzer.Event list, RemoveQuizzer.Error>>
        | SetName of string
        | CancelNewQuizzer

    type ExternalMessage =
        | Error of string
        | WorkflowSuccess of RunQuizEvent list
        | NoOp

    let init roster =
        { Roster = roster; NewQuizzer = None }, Cmd.none

    let update message model =
        match message with
        | Message.Close -> None, Cmd.none, ExternalMessage.NoOp


    let render model dispatch =
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
                    div { "TBD" }
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
