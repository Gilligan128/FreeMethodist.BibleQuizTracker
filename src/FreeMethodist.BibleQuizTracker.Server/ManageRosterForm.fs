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

    type ModelNewQuizzer =
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
          NewQuizzer: ModelNewQuizzer option }

    type AsyncOperationOfCapacity<'success, 'error> =
        AsyncOperationStatus<unit -> AsyncResult<'success, 'error>, Result<'success, 'error>>

    type Message =
        | Close
        | NewQuizzer of ModelNewQuizzer
        | AddQuizzer of AsyncOperationOfCapacity<QuizzerParticipating, AddQuizzer.Error>
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
                    | RemoveQuizzer.QuizStateError _ -> "Quiz is in the wrong state"
                    | RemoveQuizzer.QuizzerNotParticipating error -> $"{error} is not participating in the quiz"

                Some model, Cmd.none, ExternalMessage.Error errorText
        | Message.NewQuizzer modelNewQuizzer ->
            Some
                { model with
                    NewQuizzer = Some modelNewQuizzer },
            Cmd.none,
            ExternalMessage.NoOp
        | Message.AddQuizzer(Started cap) ->
            let cmd = cap () |> mapToAsyncOperationCmd Message.AddQuizzer
            Some model, cmd, ExternalMessage.NoOp
        | Message.AddQuizzer(Finished result) ->
            match result with
            | Ok events ->
                let events = RunQuizEvent.QuizzerParticipating events
                Some { model with NewQuizzer = None }, Cmd.none, ExternalMessage.WorkflowSuccess [ events ]
            | Result.Error error ->
                let errorText =
                    match error with
                    | AddQuizzer.DbError(error) -> error |> mapDbErrorToString
                    | AddQuizzer.QuizState _ -> "Quiz is in the wrong state"
                    | AddQuizzer.QuizzerAlreadyAdded error -> $"{error} is not participating in the quiz"

                Some model, Cmd.none, ExternalMessage.Error errorText
        | Message.SetName s ->
            let newQuizzer =
                match model.NewQuizzer with
                | Some(ModelNewQuizzer.Individual _) -> Some(ModelNewQuizzer.Individual s)
                | Some(ModelNewQuizzer.Team(_, position)) -> Some(ModelNewQuizzer.Team(s, position))
                | None -> None

            Some { model with NewQuizzer = newQuizzer }, Cmd.none, ExternalMessage.NoOp
        | Message.CancelNewQuizzer -> Some { model with NewQuizzer = None }, Cmd.none, ExternalMessage.NoOp

    let private renderQuizzers removeCap dispatch quizzers =
        ul {
            for quizzer in quizzers do
                let removeCap = removeCap |> fun cap -> cap quizzer

                li {

                    div {
                        attr.``class`` "columns is-mobile"

                        div {
                            attr.``class`` "column has-background-info-light"

                            text quizzer
                        }

                        div {
                            attr.``class`` "column"
                            "data-tooltip" => $"Remove {quizzer}"
                            button {
                                attr.``class`` "delete is-medium"
                               
                                removeCap |> Html.disabledIfNone

                                on.click (fun _ ->
                                    removeCap
                                    |> Option.iter (fun cap -> cap |> Started |> RemoveQuizzer |> dispatch))
                            }
                        }
                    }
                }
        }

    let private renderTeam (renderQuizzers: Quizzer list -> Node) dispatch newQuizzer (team, position) =
        div {
            attr.``class`` "columns"

            div {
                attr.``class`` "column"

                h3 {
                    attr.``class`` "title is-4"
                    text team.Name
                }

                div {
                    attr.``class`` "box"

                    div {
                        attr.``class`` "block"
                        renderQuizzers team.Quizzers
                    }

                    div {
                        attr.``class`` "block"
                        "data-tooltip" => "Add a quizzer"
                        match newQuizzer with
                        | None ->
                            button {
                                attr.``class`` "button is-info is-light"

                                on.click (fun _ -> dispatch (Message.NewQuizzer(ModelNewQuizzer.Team("", position))))

                                span {
                                    attr.``class`` "icon"
                                    i { attr.``class`` "fas fa-solid fa-plus" }
                                }
                            }
                        | Some _ -> empty ()
                    }

                }
            }
        }

    let private renderTeams (renderTeam: TeamRoster * TeamPosition -> Node) (teamRoster1, teamRoster2) =
        div {
            attr.``class`` "columns"

            div {
                attr.``class`` "column"

                renderTeam (teamRoster1, TeamPosition.TeamOne)
            }

            div {
                attr.``class`` "column"

                renderTeam (teamRoster2, TeamPosition.TeamTwo)
            }
        }

    let private renderAddQuizzerForm addQuizzerCap dispatch (name, teamPosOpt) =
        div {
            attr.``class`` "field has-addons"

            p {
                attr.``class`` "control is-expanded"

                input {
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.placeholder "Quizzer Name"

                    bind.input.string name (fun e -> dispatch (Message.SetName e))
                }
            }

            p {
                attr.``class`` "control"

                button {
                    attr.``class`` "button is-info"
                    let addQuizzerData: AddQuizzer.Data = { Name = name; Team = teamPosOpt }

                    addQuizzerCap
                    |> Option.map (fun cap -> (fun () -> cap addQuizzerData))
                    |> Option.map (fun cap -> on.click (fun _ -> dispatch (Message.AddQuizzer(Started(cap)))))
                    |> Option.defaultValue (attr.empty ())

                    text "Save"
                }
            }

            p {
                attr.``class`` "control"

                button {
                    attr.``class`` "button is-danger"
                    on.click (fun _ -> dispatch Message.CancelNewQuizzer)
                    text "Cancel"
                }
            }
        }

    let render capabilities (model: Model option) dispatch =
        let removeCap quizzer =
            model
            |> Option.bind (fun model -> capabilities.RemoveQuizzer)
            |> Option.map (fun remove -> fun () -> remove { Quizzer = quizzer })

        let renderQuizzers = renderQuizzers removeCap dispatch

        let renderTeam =
            renderTeam renderQuizzers dispatch (model |> Option.bind (fun m -> m.NewQuizzer))

        let renderAddQuizzer = renderAddQuizzerForm capabilities.AddQuizzer dispatch

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
                            div {
                                attr.``class`` "block"
                                renderQuizzers quizzers
                            }

                            match model.NewQuizzer with
                            | None ->
                                div {
                                    attr.``class`` "block"

                                    button {
                                        attr.``class`` "button is-info is-light"

                                        on.click (fun _ -> dispatch (Message.NewQuizzer(ModelNewQuizzer.Individual "")))

                                        span {
                                            attr.``class`` "icon"
                                            i { attr.``class`` "fas fa-solid fa-plus" }
                                        }
                                    }
                                }
                            | Some _ -> empty ()

                        match model.NewQuizzer with
                        | Some(ModelNewQuizzer.Individual name) -> renderAddQuizzer (name, None)
                        | None -> empty ()
                        | Some(ModelNewQuizzer.Team(name, teamPos)) -> renderAddQuizzer (name, Some teamPos)





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
