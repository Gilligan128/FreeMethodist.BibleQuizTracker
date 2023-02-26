module FreeMethodist.BibleQuizTracker.Server.CreateQuizForm

open System
open System.Text.RegularExpressions
open Bolero
open Bolero.Builders
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Tournament
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.CreateQuiz.Workflow
open FreeMethodist.BibleQuizTracker.Server.CreateQuiz.Pipeline
open InputFields

module CreateQuizForm =
    type ModalForm<'T> =
        | Inert
        | Active of 'T
        | Submitting of 'T

    type TournamentFormData =
        { Name: string
          Church: string
          Room: string
          Round: string
          GradeDivision: GradeDivision option
          CompetitionDivision: CompetitionDivision option }

    type CreateQuizFormData =
        { Code: QuizCode
          CompetitionStyle: CompetitionStyle
          TournamentInfo: TournamentFormData
          Error: string option }

    type Model = ModalForm<CreateQuizFormData>

    type Message =
        | Submit of AsyncOperationStatus<unit, Result<CreateQuiz.Event list, string>>
        | SetCode of string
        | SetCompetitionStyle of CompetitionStyle
        | SetTeamOneName of string
        | SetTeamTwoName of string
        | SetTournamentName of string
        | SetTournamentChurch of string
        | SetTournamentRoom of string
        | SetTournamentRound of string
        | SetTournamentGradeDivision of GradeDivision option
        | SetTournamentCompetitionDivision of CompetitionDivision option
        | Start
        | Cancel

    let private stringToOption (s: string) =
        if String.IsNullOrEmpty(s) then None else Some s

    let init = Inert, Cmd.none

    let updateCompetitionStyleWithTeamName competitionStyle teamPosition name =
        match teamPosition with
        | TeamOne -> Team { competitionStyle with TeamOneName = name }
        | TeamTwo -> Team { competitionStyle with TeamTwoName = name }

    let updateFormWithTeamName formData teamPosition name =
        match formData.CompetitionStyle with
        | Individual -> None
        | Team teamStyle ->
            let newTeamModel = name |> updateCompetitionStyleWithTeamName teamStyle teamPosition

            Active { formData with CompetitionStyle = newTeamModel } |> Some

    let update generateCode saveNewQuiz spectateQuiz message model =
        match message, model with
        | Start, Inert ->
            Active
                { Code = generateCode ()
                  CompetitionStyle = Team { TeamOneName = ""; TeamTwoName = "" }
                  Error = None
                  TournamentInfo =
                    { Name = ""
                      Church = ""
                      Room = ""
                      Round = ""
                      GradeDivision = None
                      CompetitionDivision = None } },
            Cmd.none
        | SetTeamOneName name, Active quizFormData ->
            let model =
                name |> updateFormWithTeamName quizFormData TeamOne |> Option.defaultValue model

            model, Cmd.none
        | SetTeamTwoName name, Active quizFormData ->
            let model =
                name |> updateFormWithTeamName quizFormData TeamTwo |> Option.defaultValue model

            model, Cmd.none
        | Submit (Started _), Active activeModel ->
            let mapToFinished result = result |> Finished |> Submit

            let mapErrorToString (error: CreateQuiz.Error) =
                match error with
                | CreateQuiz.Error.DbError er -> er |> mapDbErrorToString
                | CreateQuiz.Error.RemoteError er -> er
                | CreateQuiz.Error.CodeAlreadyExists er -> $"Quiz code {er} already exists"
                | CreateQuiz.Error.IndividualCompetitionStyle -> "Individual competition style is not yet implemented"

            let cmd =
                { Code = activeModel.Code
                  CompetitionStyle = activeModel.CompetitionStyle
                  TournamentInfo =
                    { Link =
                        activeModel.TournamentInfo.Name
                        |> stringToOption
                        |> Option.map TournamentLink.Name
                      Church = activeModel.TournamentInfo.Church |> stringToOption
                      Room = activeModel.TournamentInfo.Room |> stringToOption
                      Round = activeModel.TournamentInfo.Round |> stringToOption
                      GradeDivision = activeModel.TournamentInfo.GradeDivision
                      CompetitionDivision = activeModel.TournamentInfo.CompetitionDivision } }
                |> createQuiz saveNewQuiz
                |> AsyncResult.mapError mapErrorToString
                |> Async.timeoutNone 5000
                |> Async.map (Option.defaultValue (Result.Error "Creating the quiz timed out"))
                |> Async.map mapToFinished
                |> Cmd.OfAsync.result

            Submitting activeModel, cmd

        | Submit (Finished (Result.Error message)), Submitting formData ->
            Active { formData with Error = Some message }, Cmd.none
        | Submit (Finished _), Active formData
        | Submit (Finished _), Submitting formData ->
            spectateQuiz formData.Code
            Inert, Cmd.none
        | Start, Active _ -> model, Cmd.none
        | _, Submitting _ -> model, Cmd.none
        | Cancel, _ -> Inert, Cmd.none
        | _, Inert -> model, Cmd.none
        | SetCode code, Active formData -> Active { formData with Code = code }, Cmd.none
        | SetCompetitionStyle competitionStyle, Active quizFormData ->
            let newCompetitionStyle newStyle existingStyle =
                match existingStyle, newStyle with
                | CompetitionStyle.Individual, CompetitionStyle.Team _
                | CompetitionStyle.Team _, CompetitionStyle.Individual -> newStyle
                | CompetitionStyle.Team _, CompetitionStyle.Team _
                | CompetitionStyle.Individual, CompetitionStyle.Individual -> existingStyle

            Active
                { quizFormData with
                    CompetitionStyle = newCompetitionStyle competitionStyle quizFormData.CompetitionStyle },
            Cmd.none
        | SetTournamentName name, Active model ->
            Active { model with TournamentInfo = { model.TournamentInfo with Name = name } }, Cmd.none
        | SetTournamentChurch church, Active model ->
            Active { model with TournamentInfo = { model.TournamentInfo with Church = church } }, Cmd.none
        | SetTournamentRoom room, Active model ->
            Active { model with TournamentInfo = { model.TournamentInfo with Room = room } }, Cmd.none
        | SetTournamentRound round, Active model ->
            Active { model with TournamentInfo = { model.TournamentInfo with Round = round } }, Cmd.none
        | SetTournamentGradeDivision gradeDivision, Active model ->
            Active { model with TournamentInfo = { model.TournamentInfo with GradeDivision = gradeDivision } }, Cmd.none
        | SetTournamentCompetitionDivision competitionDivision, Active model ->
            Active
                { model with TournamentInfo = { model.TournamentInfo with CompetitionDivision = competitionDivision } },
            Cmd.none


    let competitionStyleView formData dispatch : Node =
        cond formData.CompetitionStyle
        <| function
            | Individual -> empty ()
            | Team teamData ->
                concat {
                    div {
                        attr.``class`` "field"

                        label {
                            attr.``class`` "label"
                            "Team One: "
                        }

                        div {
                            attr.``class`` "control"

                            input {
                                attr.``class`` "input"
                                attr.``type`` "text"

                                bind.input.string teamData.TeamOneName (fun name ->
                                    dispatch <| Message.SetTeamOneName name)
                            }
                        }
                    }

                    div {
                        attr.``class`` "field"

                        label {
                            attr.``class`` "label"
                            "Team Two: "
                        }

                        div {
                            attr.``class`` "control"

                            input {
                                attr.``class`` "input"
                                attr.``type`` "text"

                                bind.input.string teamData.TeamTwoName (fun name ->
                                    dispatch <| Message.SetTeamTwoName name)
                            }
                        }
                    }
                }

    let activeView ((formData: CreateQuizFormData), isSubmitting) dispatch : Node =
        div {
            attr.``class`` "modal is-active"
            div { attr.``class`` "modal-background" }

            div {
                attr.``class`` "modal-card"

                header {
                    attr.``class`` "modal-card-head"

                    p {
                        attr.``class`` "modal-card-title"
                        "Create Quiz"
                    }

                    button {
                        attr.``class`` "delete"
                        attr.aria "label" "close"

                        attr.disabled (if isSubmitting then "disabled" else null)

                        on.click (fun _ -> dispatch <| Cancel)
                    }
                }

                section {
                    attr.``class`` "modal-card-body"

                    div {
                        attr.``class`` "field"

                        div {
                            attr.``class`` "control"

                            label {
                                attr.``class`` "label"
                                "Code:"
                            }

                            input {
                                attr.``class`` "input"

                                bind.input.string formData.Code (fun code -> dispatch <| SetCode code)
                            }
                        }

                    }

                    div {
                        attr.``class`` "field"

                        label {
                            attr.``class`` "label"

                            "Competition Style:"
                        }

                        div {
                            attr.``class`` "control"

                            label {
                                attr.``class`` "radio"

                                input {
                                    attr.``type`` "radio"
                                    attr.name "competitionstyle"

                                    bind.change.string ("Team") (fun _ ->
                                        dispatch
                                        <| Message.SetCompetitionStyle(Team { TeamOneName = ""; TeamTwoName = "" }))
                                }

                                "Team"
                            }

                            label {
                                attr.``class`` "radio"

                                input {
                                    attr.``type`` "radio"
                                    attr.name "competitionstyle"

                                    bind.change.string ("Individuals") (fun _ ->
                                        dispatch <| Message.SetCompetitionStyle(Individual))
                                }

                                "Individuals"
                            }
                        }
                    }

                    competitionStyleView formData dispatch

                    fieldset {
                        attr.``class`` "box"

                        legend {

                            "Tournament Info (optional)"
                        }

                        labeledField "Tournament" formData.TournamentInfo.Name (fun name ->
                            dispatch <| SetTournamentName name)

                        labeledField "Church" formData.TournamentInfo.Church (fun church ->
                            dispatch <| SetTournamentChurch church)

                        div {
                            attr.``class`` "field is-grouped"

                            bulmaControl (
                                concat {
                                    bulmaLabel "Room"

                                    bulmaInput
                                        (fun room -> dispatch <| SetTournamentRoom room)
                                        formData.TournamentInfo.Room
                                }
                            )

                            bulmaControl (
                                concat {
                                    bulmaLabel "Round"

                                    bulmaInput
                                        (fun round -> dispatch <| SetTournamentRound round)
                                        formData.TournamentInfo.Round
                                }
                            )
                        }

                        div {
                            attr.``class`` "field is-grouped"

                            bulmaControl (
                                concat {
                                    bulmaLabel "Competition Division"

                                    bulmaSelect
                                        (fun competitionDivision ->
                                            match competitionDivision with
                                            | "Rookie" -> Some CompetitionDivision.Rookie
                                            | "Veteran" -> Some CompetitionDivision.Veteran
                                            | _ -> None
                                            |> SetTournamentCompetitionDivision
                                            |> dispatch)
                                        ([ ("", "None"); ("Rookie", "Rookie"); ("Veteran", "Veteran") ])
                                        (formData.TournamentInfo.CompetitionDivision  |> Option.map string |> Option.defaultValue "")
                                }
                            )

                            bulmaControl (
                                concat {
                                    bulmaLabel "Grade Division"

                                    bulmaSelect
                                        (fun gradeDivision ->
                                            match gradeDivision with
                                            | "YoungTeen" -> Some GradeDivision.YoungTeen
                                            | "SeniorTeen" -> Some GradeDivision.SeniorTeen
                                            | "Kids" -> Some GradeDivision.Kids
                                            | "QUIC" -> Some GradeDivision.QUIC
                                            | _ -> None
                                            |> SetTournamentGradeDivision
                                            |> dispatch)
                                        ([ ("", "None")
                                           ("YoungTeen", "Young Teen")
                                           ("SeniorTeen", "Senior Teen")
                                           ("Kids", "Kids")
                                           ("QUIC", "QUIC") ])
                                        (formData.TournamentInfo.GradeDivision |> Option.map string |> Option.defaultValue "")
                                }
                            )
                        }
                    }
                }

                div {
                    attr.``class`` (
                        match formData.Error with
                        | Some _ -> "notification is-warning"
                        | None -> "notification is-warning is-hidden"
                    )

                    text (formData.Error |> Option.defaultValue "")
                }

                footer {
                    attr.``class`` "modal-card-foot"

                    button {
                        attr.``class`` (
                            (if isSubmitting then "is-loading" else "")
                            |> fun loadingClass -> $"button is-success {loadingClass}"
                        )

                        on.click (fun _ -> Started() |> Submit |> dispatch)
                        "Submit"
                    }

                    button {
                        attr.``class`` "button"

                        attr.disabled (if isSubmitting then "disabled" else null)

                        on.click (fun _ -> Cancel |> dispatch)
                        "Cancel"
                    }


                }
            }
        }

    let view model dispatch : Node =
        match model with
        | Model.Inert -> empty ()
        | Model.Active formData -> activeView (formData, false) dispatch
        | Model.Submitting formData -> activeView (formData, true) dispatch
