module FreeMethodist.BibleQuizTracker.Server.RunningQuizPage_Render

open System
open Elmish
open Bolero
open Bolero.Html
open FreeMethodist.BibleQuizTracker.Server.Capabilities
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.RunningQuizPage_Model
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common_Page

type private quizPage = Template<"wwwroot/Quiz.html">

let prejumpMessage capability =
    capability ()
    |> AsyncResult.map (
        List.map (fun event ->
            match event with
            | Prejump.Event.Prejump quizzerPrejumped -> RunQuizEvent.QuizzerPrejumped quizzerPrejumped
            | Prejump.Event.IndividualScoreChanged individualScoreChanged ->
                RunQuizEvent.IndividualScoreChanged individualScoreChanged
            | Prejump.Event.TeamScoreChanged event -> RunQuizEvent.TeamScoreChanged event)
    )
    |> AsyncResult.mapError (fun error ->
        match error with
        | Prejump.Error.DbError error -> error |> mapDbErrorToString
        | Prejump.Error.RemoteError error -> $"Remote Error: {error}"
        | Prejump.Error.NoCurrentQuizzer _ -> "No quizzer selected"
        | Prejump.Error.WrongQuizState quizStateError -> "Quiz is not running")

let changeCurrentQuestionMessage questionNumber (capability: ChangeCurrentQuestionCap) =
    let questionNumber =
        (questionNumber |> PositiveNumber.numberOrOne)

    capability { Question = questionNumber }
    |> AsyncResult.map RunQuizEvent.CurrentQuestionChanged
    |> AsyncResult.map List.singleton
    |> AsyncResult.mapError (fun error ->
        match error with
        | ChangeCurrentQuestion.Error.QuizState error -> mapQuizStateErrorToString error
        | ChangeCurrentQuestion.DbError dbError -> mapDbErrorToString dbError)

let answerCorrectlyMessage capability =
    capability ()
    |> AsyncResult.map (
        List.map (fun event ->
            match event with
            | AnswerCorrectly.Event.CurrentQuestionChanged currentQuestionChanged ->
                RunQuizEvent.CurrentQuestionChanged currentQuestionChanged
            | AnswerCorrectly.Event.IndividualScoreChanged individualScoreChanged ->
                RunQuizEvent.IndividualScoreChanged individualScoreChanged
            | AnswerCorrectly.Event.TeamScoreChanged teamScoreChanged -> RunQuizEvent.TeamScoreChanged teamScoreChanged)
    )
    |> AsyncResult.mapError (fun error ->
        match error with
        | AnswerCorrectly.QuizzerNotFound er -> $"Quizzer {er} was not found in this quiz"
        | AnswerCorrectly.Error.QuizStateError _ -> "Quiz is not running"
        | AnswerCorrectly.Error.NoCurrentQuizzer -> "No quizzer selected"
        | (AnswerCorrectly.Error.QuizzerAlreadyAnsweredCorrectly (QuizAnswer.QuizzerAlreadyAnsweredCorrectly (quizzer,
                                                                                                              question))) ->
            $"Quizzer {quizzer} already correctly answered question {question |> PositiveNumber.value}"
        | AnswerCorrectly.Error.DbError dbError -> dbError |> mapDbErrorToString)

let quizzerView removeQuizzerCap dispatch (currentQuizzer: Quizzer option) (quizzer: QuizzerModel, jumpPosition: int) =
    let removeCap =
        removeQuizzerCap |> fun cap -> cap quizzer.Name

    quizPage
        .Quizzer()
        .Name(quizzer.Name)
        .Score(string quizzer.Score)
        .JumpOrder(jumpPosition |> string)
        .HiddenClass(
            match jumpPosition with
            | 0 -> "is-invisible"
            | _ -> ""
        )
        .RemoveButton(
            button {
                attr.``class`` "button is-info is-light"

                removeCap |> Html.disabledIfNone

                on.click (fun _ ->
                    removeCap
                    |> Option.iter (fun cap -> cap |> Started |> RemoveQuizzer |> dispatch))

                span {
                    attr.``class`` "icon"
                    i { attr.``class`` "fas fa-times-circle" }
                }
            }
        )
        .Select(fun _ -> dispatch (SelectQuizzer(Started quizzer.Name)))
        .BackgroundColor(
            currentQuizzer
            |> Option.map (fun current ->
                if quizzer.Name = current then
                    "has-background-grey-lighter"
                else
                    "has-background-white-ter")
            |> (fun current ->
                match current with
                | None -> ""
                | Some q -> q)
        )
        .AnswerColor(
            match quizzer.AnswerState with
            | DidNotAnswer -> ""
            | AnsweredCorrectly -> "success"
            | AnsweredIncorrectly -> "danger"
        )
        .AppealVisible(
            match quizzer.AppealState with
            | NoFailure -> "is-hidden"
            | AppealFailure -> ""
        )
        .PrejumpBadgeVisible(
            match quizzer.PrejumpState with
            | NoPrejump -> "is-hidden"
            | Prejumped -> "")
        .Elt()

let private getJumpPosition jumpOrder (quizzer: QuizzerModel) =
    jumpOrder
    |> Seq.tryFindIndex (fun q -> q = quizzer.Name)
    |> Option.map ((+) 1)
    |> function
        | Some v -> v
        | None -> 0

let private teamView
    position
    (quizzerView: QuizzerModel * int -> Node)
    ((teamModel, jumpOrder): TeamModel * string list)
    (dispatch: Dispatch<Message>)
    =

    quizPage
        .Team()
        .Name(teamModel.Name)
        .TeamColor(
            match position with
            | TeamOne -> "success"
            | TeamTwo -> "danger"
        )
        .ScoreReadOnly(string teamModel.Score)
        .Quizzers(
            concat {
                for quizzer in teamModel.Quizzers do
                    let jumpPosition =
                        quizzer |> getJumpPosition jumpOrder

                    quizzerView (quizzer, jumpPosition)
            }
        )
        .Elt()

let individualSideView removeQuizzerCap quizzerView (jumpOrder: string list) quizzerModels =
    forEach quizzerModels
    <| fun quizzer ->
        let jumpPosition =
            quizzer |> getJumpPosition jumpOrder

        quizzerView (quizzer, jumpPosition)



let private mapItemizedTeam (team: TeamModel) : ItemizedTeam =
    { Name = team.Name
      Quizzers = team.Quizzers |> List.map (fun q -> q.Name) }

let sideViewSplit (individualsView: QuizzerModel list -> Node) index quizzerModels =
    quizzerModels
    |> List.splitInto 2
    |> List.tryItem index
    |> Option.defaultValue []
    |> individualsView

let render linkToQuiz (model: Model) (dispatch: Dispatch<Message>) =

    let isTeam model teamOneValue teamTwoValue =
        match model.AddQuizzer with
        | Inert -> false
        | AddQuizzerModel.Active (_, TeamOne) -> teamOneValue
        | AddQuizzerModel.Active (_, TeamTwo) -> teamTwoValue

    match model.Info with
    | Deferred.NotYetStarted -> p { $"Quiz {model.Code} has not yet been loaded" }
    | InProgress -> p { $"Quiz {model.Code} is loading..." }
    | Resolved resolved ->

        let removeQuizzerCap quizzer =
            resolved.Capabilities.RemoveQuizzer
            |> Option.map (fun remove -> fun () -> remove { Quizzer = quizzer })

        let quizzerView =
            quizzerView removeQuizzerCap dispatch resolved.CurrentQuizzer

        let individualSideView =
            individualSideView removeQuizzerCap quizzerView resolved.JumpOrder

        let nextQuestionCap capability =
            fun () ->
                let questionNumber =
                    resolved.CurrentQuestion + 1

                capability
                |> changeCurrentQuestionMessage questionNumber

        let previousQuestionCap capability =
            fun () ->
                let questionNumber =
                    Math.Max(resolved.CurrentQuestion - 1, 1)

                capability
                |> changeCurrentQuestionMessage questionNumber

        let potentiallyDispatchWorkflow workflowOpt =
            workflowOpt
            |> Option.iter (fun workflow -> workflow |> Started |> ExecuteWorkflow |> dispatch)

        quizPage()
            .QuizCode(model.Code)
            .QuizUrl(linkToQuiz <| model.Code)
            .CurrentUser(
                match model.User with
                | Quizmaster -> "Quizmaster"
                | Spectator -> "Spectator"
                | Quizzer name -> name
                | Scorekeeper -> "Scorekeeper"
            )
            .SideOne(
                match resolved.CompetitionStyle with
                | LoadedCompetitionStyle.Team (teamOne, teamTwo) ->
                    teamView TeamPosition.TeamOne quizzerView (teamOne, resolved.JumpOrder) dispatch
                | LoadedCompetitionStyle.Individuals quizzerModels ->
                    quizzerModels
                    |> sideViewSplit individualSideView 0
            )
            .SideTwo(
                match resolved.CompetitionStyle with
                | LoadedCompetitionStyle.Team (teamOne, teamTwo) ->
                    teamView TeamPosition.TeamTwo quizzerView (teamTwo, resolved.JumpOrder) dispatch
                | LoadedCompetitionStyle.Individuals quizzerModels ->
                    quizzerModels
                    |> sideViewSplit individualSideView 1
            )
            .CurrentQuestion(string resolved.CurrentQuestion)
            .NextQuestion(fun _ ->
                resolved.Capabilities.ChangeCurrentQuestion
                |> Option.iter (fun cap -> dispatch (ExecuteWorkflow(Started(nextQuestionCap cap)))))
            .UndoQuestion(fun _ ->
                resolved.Capabilities.ChangeCurrentQuestion
                |> Option.iter (fun cap -> dispatch (ExecuteWorkflow(Started(previousQuestionCap cap)))))
            .CurrentQuizzer(
                match resolved.CurrentQuizzer with
                | Some q -> $"{q}'s Turn"
                | None -> ""
            )
            .JumpLockToggleAction(
                match resolved.JumpState with
                | Locked -> "Unlock"
                | Unlocked -> "Lock"
            )
            .AddQuizzerTeamView(
                match resolved.CompetitionStyle with
                | LoadedCompetitionStyle.Individuals _ -> Html.empty ()
                | LoadedCompetitionStyle.Team (teamOne, teamTwo) ->
                    quizPage
                        .AddQuizzerTeam()
                        .TeamOneName(teamOne.Name)
                        .TeamTwoName(teamTwo.Name)
                        .SetAddQuizzerTeamOne(fun _ -> dispatch (AddQuizzer(SetTeam TeamOne)))
                        .SetAddQuizzerTeamTwo(fun _ -> dispatch (AddQuizzer(SetTeam TeamTwo)))
                        .AddQuizzerIsTeamOne(isTeam resolved true false)
                        .AddQuizzerIsTeamTwo(isTeam resolved false true)
                        .Elt()
            )
            .AddQuizzerName(
                (match resolved.AddQuizzer with
                 | RunningQuizPage_Model.Active (name, _) -> name
                 | Inert -> ""),
                (fun name ->
                    dispatch (
                        AddQuizzerMessage.SetName name
                        |> Message.AddQuizzer
                    ))
            )
            .AddQuizzerStart(fun _ -> dispatch (AddQuizzer Start))
            .AddQuizzerCancel(fun _ -> dispatch (AddQuizzer Cancel))
            .AddQuizzerActive(
                if resolved.AddQuizzer = Inert then
                    ""
                else
                    "is-active"
            )
            .AddQuizzerSubmit(fun _ -> dispatch (AddQuizzer(Submit(Started()))))
            .AnswerIncorrectly(fun _ -> dispatch (AnswerIncorrectly(Started())))
            .AnswerCorrectly(fun _ ->
                resolved.Capabilities.AnswerCorrectly
                |> Option.map (fun cap -> fun () -> answerCorrectlyMessage cap)
                |> potentiallyDispatchWorkflow)
            .FailAppeal(fun _ -> dispatch (FailAppeal(Started())))
            .ClearAppeal(fun _ -> dispatch (ClearAppeal(Started())))
            .ItemizedScore(
                ItemizedScore.render
                    { CompetitionStyle =
                        match resolved.CompetitionStyle with
                        | LoadedCompetitionStyle.Team (teamOne, teamTwo) ->
                            (mapItemizedTeam teamOne, mapItemizedTeam teamTwo)
                            |> ItemizedCompetitionStyle.Team
                        | LoadedCompetitionStyle.Individuals quizzers ->
                            quizzers
                            |> List.map (fun q -> q.Name)
                            |> ItemizedCompetitionStyle.Individual
                      NumberOfQuestions = resolved.NumberOfQuestions
                      QuestionsWithEvents = resolved.QuestionScores }
                    dispatch
            )
            .CompleteQuiz(fun _ -> dispatch (CompleteQuiz(Started())))
            .ReopenQuiz(fun _ -> dispatch (ReopenQuiz(Started())))
            .PrejumpActionVisibility(
                match resolved.Capabilities.Prejump with
                | Some _ -> ""
                | None -> "is-hidden"
            )
            .Prejump(fun _ ->
                resolved.Capabilities.Prejump
                |> Option.map (fun cap -> fun () -> prejumpMessage cap)
                |> potentiallyDispatchWorkflow)
            .Elt()
