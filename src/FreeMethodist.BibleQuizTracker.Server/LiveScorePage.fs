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

    let private loadCompletedQuizzer (quizzer: CompletedQuizzer) : LiveScoreQuizzer =
        { Name = quizzer.Name
          Score = quizzer.Score }

    let private loadCompleteTeam (team: CompletedTeam) : LiveScoreTeam =
        { Name = team.Name
          Score = team.Score
          Quizzers = team.Quizzers |> List.map loadCompletedQuizzer }

    let private loadRunningQuizzer (quizzer: QuizzerState) : LiveScoreQuizzer =
        { Name = quizzer.Name
          Score = quizzer.Score }

    let loadRunningTeam (team: QuizTeamState) : LiveScoreTeam =
        { Name = team.Name
          Score = team.Score
          Quizzers = team.Quizzers |> List.map loadRunningQuizzer }

    let private loadFromQuiz quiz =
        match quiz with
        | Quiz.Completed quizState ->
            { LastUpdated = DateTimeOffset.Now
              QuestionState = Completed quizState.CompletedQuestions.Length
              CompetitionStyle =
                LiveScoreCompetitionStyle.Team(
                    (loadCompleteTeam quizState.winningTeam),
                    (loadCompleteTeam quizState.losingTeam)
                ) }
        | Running quizState ->
            { LastUpdated = DateTimeOffset.Now
              QuestionState = Current quizState.CurrentQuestion
              CompetitionStyle =
                LiveScoreCompetitionStyle.Team((loadRunningTeam quizState.TeamOne), (loadRunningTeam quizState.TeamTwo)) }
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
            let loaded =
                quiz |> Option.map loadFromQuiz |> Ok |> Resolved

            { model with Scores = loaded }, Cmd.none
        | Initialize (Finished (Error error)) -> model, Cmd.none
        | OnQuizEvent event ->
            match model.Scores with
            | NotYetStarted -> model, Cmd.none
            | InProgress -> model, Cmd.none
            | Resolved (Ok (Some loaded)) ->
                { model with Scores = Resolved(Ok (Some { loaded with LastUpdated = DateTimeOffset.Now })) }, Cmd.none
            | Resolved (Ok None) ->
                { model with Scores = Resolved(Ok None) }, Cmd.none
            | Resolved (Error _) -> model, Cmd.none //silently fail for now.

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

    let teamScoreView (model: LiveScoreTeam) (bgColor) : Node =
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
        | Resolved (Ok (None)) -> h1 {$"Quiz {model.Code} was not found"}
        | Resolved (Ok (Some loaded)) ->
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

                            cond loaded.QuestionState
                            <| function
                                | Current current -> h1 { $"Question: {current |> PositiveNumber.value}" }
                                | Completed completed -> h1 { $"{completed} Questions" }

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
                                    teamScoreView teamOne ("success")
                                    teamScoreView teamTwo ("danger")
                                }
                            | LiveScoreCompetitionStyle.Individual individualStyle -> empty ()

                    }
                }

            }
