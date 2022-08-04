namespace FreeMethodist.BibleQuizTracker.Server

open System
open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

module LiveScorePage =

    type Message =
        | Initialize of AsyncOperationStatus<unit, Result<Quiz option, DbError>>
        | OnQuizEvent of RunQuizEvent

    let init quizCode =
        { Code = quizCode
          Scores = NotYetStarted },
        (Cmd.ofMsg (Message.Initialize(Started())))

    let private loadCompletedQuizzer (quizzer : CompletedQuizzer): LiveScoreQuizzer =
        { Name = quizzer.Name
          Score = quizzer.Score }

    let private loadCompleteTeam (team: CompletedTeam) : LiveScoreTeam =
        { Name = team.Name
          Score = team.Score
          Quizzers = team.Quizzers |> List.map loadCompletedQuizzer }

    let private loadFromQuiz quiz =
        match quiz with
        | Quiz.Completed quizState ->
            { LastUpdated = DateTimeOffset.Now
              QuestionState = Completed quizState.CompletedQuestions.Length
              CompetitionStyle = LiveScoreCompetitionStyle.Team ((loadCompleteTeam quizState.winningTeam),(loadCompleteTeam quizState.losingTeam)) }
        | Running quizState -> Unchecked.defaultof<LiveScores>
        | Official quizState -> Unchecked.defaultof<LiveScores>

    let update connectToQuizEvents tryGetQuiz (model: LiveScoreModel) message =
        match message with
        | Initialize (Started _) ->
            let handleQuizEvent dispatch event =
                dispatch (OnQuizEvent event) |> Async.retn

            let cmd =
                connectToQuizEvents handleQuizEvent (model.Code, None)
                |> Cmd.ofSub

            let loadQuizCmd =
                model.Code
                |> tryGetQuiz
                |> Async.map (Initialize << Finished)
                |> Cmd.OfAsync.result

            { model with Scores = InProgress }, Cmd.batch [ cmd; loadQuizCmd ]

        | Initialize (Finished (Ok quiz)) ->
            let model =
                { model with
                    Scores =
                        { LastUpdated = DateTimeOffset.Now
                          QuestionState = Current PositiveNumber.one
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
                            ) }
                        |> Ok
                        |> Resolved }

            model, Cmd.none
        | Initialize (Finished (Error error)) -> model, Cmd.none
        | OnQuizEvent event ->
            match model.Scores with
            | NotYetStarted -> model, Cmd.none
            | InProgress -> model, Cmd.none
            | Resolved (Ok loaded) ->
                { model with Scores = Resolved(Ok { loaded with LastUpdated = DateTimeOffset.Now }) }, Cmd.none
            | Resolved (Error _) -> model, Cmd.none //silently fail for now. I Should wire up telemetry soon.

    let quizzerScoreView (model: LiveScoreQuizzer) : Node =
        div {
            attr.``class`` "box has-background-grey-white-ter my-2"
            attr.id $"quizzer-box-{model.Name}"

            div {
                attr.``class`` "columns is-mobile"

                div {
                    attr.``class`` "column"
                    attr.id $"quizzer-name-{model.Name}"
                    h4 { $"{model.Name}:" }
                }

                div {
                    attr.``class`` "column is-one-third"
                    attr.id $"quizzer-score-{model.Name}"
                    h4 { text (model.Score |> TeamScore.toString) }
                }
            }
        }

    let teamScoreView (model: LiveScoreTeam) (bgColor, textColor) : Node =
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
                            attr.``class`` "column is-one-third"
                            attr.id $"team-score-{model.Name}"
                            h1 { text (model.Score |> TeamScore.toString) }
                        }
                    }

                }

                forEach model.Quizzers <| quizzerScoreView
            }
        }

    let page model : Node =
        match model.Scores with
        | NotYetStarted -> h1 { "Not yet loaded" }
        | InProgress -> h1 { "Loading..." }
        | Resolved (Error error) -> h1 { $"There was an error while loading: {error}" }
        | Resolved (Ok loaded) ->
            concat {
                div {
                    attr.``class`` "content"

                    div {
                        attr.``class`` "box has-background-white-ter columns"

                        div {
                            attr.``class`` "column"
                            h1 { $"Quiz: {model.Code}" }
                        }

                        div {
                            attr.``class`` "column"
                            cond loaded.QuestionState <| function
                                | Current current -> h1 { $"Question: {current |> PositiveNumber.value}" }
                                | Completed completed -> h1 { $"{completed} Questions"  }
                            
                        }

                        div {
                            attr.``class`` "column"
                            h4 { $"Last update: {loaded.LastUpdated}" }
                        }
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
