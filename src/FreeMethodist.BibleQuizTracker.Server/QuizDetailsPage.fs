module FreeMethodist.BibleQuizTracker.Server.QuizDetailsPage

open Bolero
open Bolero.Html
open Elmish
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView
open FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView.ItemizedScore
open FreeMethodist.BibleQuizTracker.Server.Routing
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows

type QuizControlCapabilityProvider = {
   CompleteQuiz : Quiz -> CompleteQuizCap option
   ReopenQuiz : Quiz -> ReopenQuizCap option
   Spectate : Quiz -> Link option
   LiveScore : Quiz -> Link option
}

let init quizCode =
    { Code = quizCode
      Details = NotYetStarted },
    Cmd.ofMsg (Initialize(Started()))

let subOfFunc arg (func: 'a -> unit) : Sub<QuizDetailsMessage> = fun _ -> func arg

let loadRunningTeam (team: QuizTeamState) : ItemizedTeam =
    { Name = team.Name
      Quizzers = team.Quizzers |> List.map (fun q -> q.Name) }

let private loadRunningQuiz quiz =
    { NumberOfQuestions = quiz.Questions |> Map.keys |> Seq.max
      QuestionsWithEvents =
        quiz.Questions
        |> Map.map (fun k v -> (v.AnswerState, v.FailedAppeal))
        |> ItemizedScoreModel.refreshQuestionScores
      CompetitionStyle = ItemizedCompetitionStyle.Team(quiz.TeamOne |> loadRunningTeam, quiz.TeamTwo |> loadRunningTeam) }

let private loadCompletedTeam (team: CompletedTeam) : ItemizedTeam =
    { Name = team.Name
      Quizzers = team.Quizzers |> List.map (fun q -> q.Name) }

let private loadCompletedQuiz (quiz: CompletedQuiz) =
    { NumberOfQuestions =
        quiz.CompletedQuestions.Length
        |> PositiveNumber.numberOrOne
      QuestionsWithEvents =
        quiz.CompletedQuestions
        |> List.indexed
        |> List.collect (fun (i, v) ->
            ItemizedScoreModel.refreshQuestionScore
                (i + 1 |> PositiveNumber.numberOrOne)
                ((QuizAnswer.Complete v.AnswerState), v.FailedAppeal))
      CompetitionStyle =
        match quiz.CompetitionStyle with
        | CompletedCompetitionStyle.Team (teamOne, teamTwo) ->
            ItemizedCompetitionStyle.Team(teamOne |> loadCompletedTeam, teamTwo |> loadCompletedTeam)
        | CompletedCompetitionStyle.Individual completedQuizzers ->
            completedQuizzers
            |> List.map (fun q -> q.Name)
            |> ItemizedCompetitionStyle.Individual }

let private availableCapabilities (provider : QuizControlCapabilityProvider) quiz : QuizControlCapabilities =
    { CompleteQuiz = provider.CompleteQuiz quiz
      ReopenQuiz = provider.ReopenQuiz quiz
      Spectate = provider.Spectate quiz
      LiveScore = provider.LiveScore quiz }
    
let private availableMessages (capabiliies :QuizControlCapabilities) =
   capabiliies.ReopenQuiz |> Option.map (fun cap -> QuizDetailsMessage.ReopenQuiz (Started ()))

let update tryGetQuiz navigate (capabilityProvider: QuizControlCapabilityProvider) (model: QuizDetailsModel) message =
    let navigateHomeCmd =
        navigate |> subOfFunc Page.Home |> Cmd.ofSub

    match message with
    | Initialize (Started _) ->
        let cmd =
            model.Code
            |> tryGetQuiz
            |> Async.map Finished
            |> Async.map Initialize
            |> Cmd.OfAsync.result

        { model with Details = InProgress }, cmd
    | Initialize (Finished (Ok (Some quiz))) ->
        let caps = availableCapabilities capabilityProvider quiz |> availableMessages
        { model with
            Details =
                Resolved(
                    match quiz with
                    | Running running ->
                        { State = nameof Running
                          ItemizedScore = running |> loadRunningQuiz }
                    | Completed completedQuiz ->
                        { State = nameof Completed
                          ItemizedScore = completedQuiz |> loadCompletedQuiz }
                    | Official _ ->
                        { State = nameof Official
                          ItemizedScore = Unchecked.defaultof<ItemizedScoreModel> }
                ) },
        Cmd.none
    | Initialize (Finished (Ok None)) -> { model with Details = NotYetStarted }, navigateHomeCmd
    | Initialize (Finished (Error error)) -> model, navigateHomeCmd



let render dispatch (model: QuizDetailsModel) : Node =
    match model.Details with
    | NotYetStarted ->
        p {
            attr.``class`` "title"
            $"Quiz {model.Code} is not yet loaded"
        }
    | InProgress ->
        div {
            progress {
                attr.``class`` "progress is-primary"
                attr.max "100"
                "15%"
            }
        }
    | Resolved loadedModel ->
        div {
            attr.``class`` "column is-two-thirds"
            div {
                attr.``class`` "columns "

                div {
                    attr.``class`` "column "

                    p {
                        attr.``class`` "title"
                        $"Quiz {model.Code} Details"
                    }

                }

                div {
                    attr.``class`` "column "

                    span {
                        attr.``class`` "tag is-success is-large"
                        loadedModel.State
                    }
                }
            }

            render loadedModel.ItemizedScore dispatch

        }
