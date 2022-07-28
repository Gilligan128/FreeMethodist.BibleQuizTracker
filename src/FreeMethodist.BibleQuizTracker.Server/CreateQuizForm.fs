module FreeMethodist.BibleQuizTracker.Server.CreateQuizForm

open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.QuizPage
open FreeMethodist.BibleQuizTracker.Server.Workflow


module CreateQuizForm =
    type ModalForm<'T> =
        | Inert
        | Active of 'T
        | Submitting of 'T

    type TeamCompetition =
        { TeamOneName: NonEmptyString
          TeamTwoName: NonEmptyString }

    type CompetitionStyle =
        | Individual
        | Team of TeamCompetition

    type CreateQuizFormData =
        { Code: QuizCode
          CompetitionStyle: CompetitionStyle
          Error: string option }

    type Model = ModalForm<CreateQuizFormData>

    type Message =
        | Submit of AsyncOperationStatus<unit, Result<unit, string>>
        | SetCode of string
        | SetCompetitionStyle of CompetitionStyle
        | SetTeamOneName of string
        | SetTeamTwoName of string
        | Start
        | Cancel
        

    let init = Inert, Cmd.none

    let updateCompetitionStyleWithTeamName competitionStyle teamPosition name =
        match teamPosition with
        | TeamOne -> Team { competitionStyle with TeamOneName = name }
        | TeamTwo -> Team { competitionStyle with TeamTwoName = name }

    let updateFormWithTeamName formData teamPosition name =
        match formData.CompetitionStyle with
        | Individual -> None
        | Team teamStyle ->
            let newTeamModel =
                name
                |> updateCompetitionStyleWithTeamName teamStyle teamPosition

            Active { formData with CompetitionStyle = newTeamModel }
            |> Some

    let update message model =
        match message, model with
        | Start, Inert ->
            Active
                { Code = ""
                  CompetitionStyle = Team { TeamOneName = ""; TeamTwoName = "" }
                  Error = None },
            Cmd.none
        | SetTeamOneName name, Active quizFormData ->
            let model =
                name
                |> updateFormWithTeamName quizFormData TeamOne
                |> Option.defaultValue model

            model, Cmd.none
        | SetTeamTwoName name, Active quizFormData ->
            let model =
                name
                |> updateFormWithTeamName quizFormData TeamTwo
                |> Option.defaultValue model

            model, Cmd.none
        | Submit (Started _), Active activeModel ->
            let cmd =
                Async.Sleep 100
                |> Async.map (fun _ -> Result.Error "Creating a quiz is not yet implemented")
                |> Async.map Finished
                |> Async.map Message.Submit
                |> Cmd.OfAsync.result

            Submitting activeModel, cmd
      
        | Submit (Finished (Result.Error message)), Submitting formData -> Active { formData with Error = Some message }, Cmd.none
        | Submit (Finished _), Active formData
        | Submit (Finished _), Submitting formData -> Inert, Cmd.none
        | Start, Active _ -> model, Cmd.none
        | _, Submitting _ -> model, Cmd.none
        | Cancel, _ -> Inert, Cmd.none
        | _, Inert -> model, Cmd.none
        | SetCode code, Active formData -> Active { formData with Code = code }, Cmd.none
        | SetCompetitionStyle competitionStyle, Active quizFormData ->
            Active { quizFormData with CompetitionStyle = competitionStyle }, Cmd.none


open CreateQuizForm

let competitionStyleView formData dispatch : Node =
     cond formData.CompetitionStyle <| function 
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
                            bind.input.string teamData.TeamOneName (fun name -> dispatch <| SetTeamOneName name)
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
                           bind.input.string teamData.TeamTwoName (fun name -> dispatch <| SetTeamTwoName name)
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

                    attr.disabled (
                        if isSubmitting then
                            "disabled"
                        else
                            null
                    )

                    on.click (fun _ -> dispatch <| Cancel)
                }
            }

            section {
                attr.``class`` "modal-card-body"

                div {
                    attr.``class`` "field"

                    label {
                        attr.``class`` "label"
                        "Code:"
                    }

                    div {
                        attr.``class`` "control"

                        input {
                            attr.``class`` "input"
                            attr.``type`` "text"
                            attr.placeholder "Quiz Code"
                            bind.input.string formData.Code (dispatch << SetCode)
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

                                bind.change.string ("Team") (fun _ -> dispatch <| SetCompetitionStyle(Team { TeamOneName = ""; TeamTwoName = "" }))
                            }
                            "Team"
                        }

                        label {
                            attr.``class`` "radio"

                            input {
                                attr.``type`` "radio"
                                attr.name "competitionstyle"
                                bind.change.string ("Individuals") (fun _ -> dispatch <| SetCompetitionStyle(Individual))
                            }

                            "Individuals"
                        }
                    }
                }          
                competitionStyleView formData dispatch
            }
            div {
                    attr.``class``  (match formData.Error with
                                     | Some _ -> "notification is-warning"
                                     | None -> "notification is-warning is-hidden")
                    text (formData.Error |> Option.defaultValue "")    
            }
            footer {
                attr.``class`` "modal-card-foot"

                button {
                    attr.``class`` (
                        (if isSubmitting then
                             "is-loading"
                         else
                             "")
                        |> fun loadingClass -> $"button is-success {loadingClass}"
                    )

                    on.click (fun _ -> Started() |> Submit |> dispatch)
                    "Submit"
                }

                button {
                    attr.``class`` "button"

                    attr.disabled (
                        if isSubmitting then
                            "disabled"
                        else
                            null
                    )

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
