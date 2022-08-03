namespace FreeMethodist.BibleQuizTracker.Server

open System
open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

module LiveScorePage =

    type Message =
        | Initialize of AsyncOperationStatus<unit, unit>
        | OnQuizEvent of RunQuizEvent

    let init quizCode =
        { Code = quizCode
          Scores = NotYetLoaded },
        (Cmd.ofMsg (Message.Initialize(Started())))

    let update connectToQuizEvents (model: LiveScoreModel) message =
        match message with
        | Initialize (Started _) ->
            let handleQuizEvent dispatch event =
                dispatch (OnQuizEvent event) |> Async.retn

            let cmd =
                connectToQuizEvents handleQuizEvent (model.Code, None)
                |> Cmd.ofSub

            { model with Scores = InProgress },
            Cmd.batch [ cmd
                        Cmd.ofMsg (Initialize(Finished())) ]
        | Initialize (Finished _) ->
            let model =
                { model with
                    Scores =
                        Loaded
                            { LastUpdated = DateTimeOffset.Now
                              CurrentQuestion = PositiveNumber.one
                              CompetitionStyle =
                                LiveScoreCompetitionStyle.Team(
                                    { Name = "Team One"
                                      Score = TeamScore.ofQuestions 2
                                      Quizzers =
                                        [ { Name = "Jack"
                                            Score = TeamScore.ofQuestions 1 }
                                          { Name = "Jill"
                                            Score = TeamScore.ofQuestions 1 }
                                          { Name = "Bob"
                                            Score = TeamScore.initial } ] },
                                    { Name = "Team Two"
                                      Score = TeamScore.ofQuestions 1
                                      Quizzers =
                                        [ { Name = "Juni"
                                            Score = TeamScore.ofQuestions 1 }
                                          { Name = "Jorge"
                                            Score = TeamScore.initial } ] }
                                ) } }

            model, Cmd.none
        | OnQuizEvent event ->
            match model.Scores with
            | NotYetLoaded -> model, Cmd.none
            | InProgress -> model, Cmd.none
            | Loaded loaded -> { model with Scores = Loaded { loaded with LastUpdated = DateTimeOffset.Now } }, Cmd.none

    let quizzerScoreView (model: LiveScoreQuizzer) : Node =
        div {
            attr.``class`` "box has-background-grey-white-ter"
            attr.id $"quizzer-box-{model.Name}"

            div {
                attr.``class`` "columns is-mobile"

                div {
                    attr.``class`` "column"
                    attr.id $"quizzer-name-{model.Name}"
                    h3 { $"{model.Name}:" }
                }

                div {
                    attr.``class`` "column"
                    attr.id $"quizzer-score-{model.Name}"
                    h3 { text (model.Score |> TeamScore.toString) }
                }
            }
        }

    let teamScoreView (model: LiveScoreTeam) (bgColor,textColor) : Node =
        div {
            attr.``class`` "column"

            div {
                attr.``class`` $"box has-background-{bgColor}"
                attr.id $"team-box-{model.Name} "

                div {
                    attr.``class`` $"box has-background-grey-lighter"
                    attr.id $"team-header-{model.Name}"

                    div {
                        attr.``class`` "columns is-mobile"

                        div {
                            attr.``class`` "column"
                            attr.id $"team-name-{model.Name}"
                            h1 { $"{model.Name}:" }
                        }

                        div {
                            attr.``class`` "column"
                            attr.id $"team-score-{model.Name}"
                            h1 { text (model.Score |> TeamScore.toString) }
                        }
                    }

                }

                forEach model.Quizzers
                <| quizzerScoreView
            }
        }

    let page model : Node =
        match model.Scores with
        | NotYetLoaded -> h1 { "Not yet loaded" }
        | InProgress -> h1 { "Loading..." }
        | Loaded loaded ->
            concat {
                h1 { $"Last Update {loaded.LastUpdated}" }

                div {
                    attr.``class`` "content"

                    div {
                        attr.``class`` "box has-text-centered has-background-white-ter"
                        h3 { $"Quiz: {model.Code}" }
                    }

                    div {
                        attr.``class`` "box has-text-centered has-background-white-bis"
                        h1 { $"Question: {loaded.CurrentQuestion |> PositiveNumber.value}" }
                    }

                    div {
                        attr.``class`` "columns"

                        cond loaded.CompetitionStyle
                        <| function
                            | LiveScoreCompetitionStyle.Team (teamOne, teamTwo) ->
                                concat {
                                    teamScoreView teamOne ("success", None)
                                    teamScoreView teamTwo ("danger", None)
                                }
                            | LiveScoreCompetitionStyle.Individual individualStyle -> empty ()

                    }
                }

            }
