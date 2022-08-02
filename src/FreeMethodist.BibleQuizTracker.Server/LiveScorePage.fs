namespace FreeMethodist.BibleQuizTracker.Server

open System
open Bolero
open Elmish
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

module LiveScorePage =

    type Message =
        | Initialize of AsyncOperationStatus<unit, unit>
        | OnQuizEvent of RunQuizEvent

    let update connectToQuizEvents (model: LiveScoreModel) message =
        match message with
        | Initialize (Started _) ->
            let handleQuizEvent dispatch event = dispatch (OnQuizEvent event)

            let cmd =
                connectToQuizEvents handleQuizEvent (model.Code, None)
                |> Cmd.ofSub

            model, cmd
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
            | Loaded loaded -> { model with Scores = Loaded { loaded with LastUpdated = DateTimeOffset.Now}}, Cmd.none

    let page model = Html.empty ()
