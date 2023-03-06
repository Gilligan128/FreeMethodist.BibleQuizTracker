module FreeMethodist.BibleQuizTracker.Server.ListQuizzesPage

open Bolero
open Elmish
open Common_Page
open Bolero.Html
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Routing
open FreeMethodist.BibleQuizTracker.Server.Tournament
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Common.Pipeline

type ExternalMessage =
    | Error of string
    | NoError

type Message = Initialize of AsyncOperationStatus<unit, Result<ListQuizItem list, DbError>>

let init =
    { StateFilter = QuizStatusFilter.All
      Quizzes = NotYetStarted },
    () |> Started |> Initialize |> Cmd.ofMsg

let update getQuizzes model message : ListQuizModel * Cmd<Message> * ExternalMessage =
    match message with
    | Initialize (Started _) ->
        let cmd =
            getQuizzes model.StateFilter
            |> Async.map (Initialize << Finished)
            |> Cmd.OfAsync.result

        { model with Quizzes = InProgress }, cmd, ExternalMessage.NoError
    | Initialize (Finished result) -> { model with Quizzes = Resolved result }, Cmd.none, NoError

let private humanReadableCompetitionDivision division =
    match division with
    | CompetitionDivision.Rookie -> "Rookie"
    | CompetitionDivision.Veteran -> "Veteran"

let private humanReadableGradeDivision division =
    match division with
    | GradeDivision.YoungTeen -> "Young Teen"
    | GradeDivision.SeniorTeen -> "Senior Teen"
    | GradeDivision.QUIC -> "QUIC"
    | GradeDivision.Kids -> "Kids"
    | GradeDivision.Custom name -> name

let render link dispatch model =
    match model.Quizzes with
    | NotYetStarted ->
        h1 {
            attr.``class`` "title"
            text "Quizzes not yet loaded"
        }
    | InProgress ->
        progress {
            attr.``class`` "progress is-primary"
            attr.max "100"
            "15%"
        }
    | Resolved (Result.Error error) -> p { text (error |> mapDbErrorToString) }
    | Resolved (Ok resolved) ->
        concat {
            h1 {
                attr.``class`` "title"
                "Quizzes"
            }

            div {
                attr.``class`` "column"

                forEach (
                    resolved
                    |> List.groupBy (fun quiz -> quiz.Tournament |> Option.defaultValue "No Tournament")
                    |> List.sortBy fst
                )
                <| fun (tournament, quizzes) ->
                    div {
                        attr.``class`` "box"

                        h2 {
                            attr.``class`` "title 4"
                            $"Tournament: {tournament}"
                        }

                        div {
                            attr.``class`` "card-content"

                            forEach quizzes
                            <| fun quiz ->
                                div {
                                    attr.``class`` "card"

                                    div {
                                        attr.``class`` "card-content"

                                        a {
                                            attr.href (link (Page.QuizDetails(quiz.Code, Router.noModel)))

                                            p {
                                      
                                                let competitionDivision =
                                                    quiz.CompetitionDivision
                                                    |> Option.map humanReadableCompetitionDivision

                                                let gradeDivision =
                                                    quiz.GradeDivision |> Option.map humanReadableGradeDivision

                                                text (
                                                    match competitionDivision, gradeDivision with
                                                    | Some competitionDivision, Some gradeDivision ->
                                                        $"{gradeDivision} {competitionDivision}"
                                                    | Some competitionDivision, None -> $"{competitionDivision}"
                                                    | None, Some gradeDivision -> $"{gradeDivision}"
                                                    | None, None -> $"Arbitrary Quiz: {quiz.Code}"
                                                )
                                            }

                                            p {

                                                text (
                                                    match quiz.CompetitionStyle with
                                                    | ListCompetitionStyle.Team (teamOne, teamTwo) ->
                                                        $"{teamOne} vs {teamTwo}"
                                                    | ListCompetitionStyle.Individual count -> $"Individuals"
                                                )
                                            }

                                            p {
                                                text (
                                                    match quiz.Room, quiz.Round with
                                                    | Some room, Some round -> $"Round: {round} Room: {room}"
                                                    | Some room, None -> $"Room: {room}"
                                                    | None, Some round -> $"Round: {round}"
                                                    | None, None -> ""
                                                )
                                            }

                                        }
                                    }
                                }
                        }
                    }
            }
        }
