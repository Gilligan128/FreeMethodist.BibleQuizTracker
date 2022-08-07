namespace FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView

open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Bolero
open Bolero.Html


module ItemizedScore =

 type private itemizedPage = Template<"wwwroot/ItemizedScore.html">
 let  render (model : ItemizedScoreModel) dispatch =
    let answerScore answerState =
        let score =
            TeamScore.initial
            |> (TeamScore.correctAnswer)
            |> TeamScore.value

        match answerState with
        | AnsweredCorrectly -> score
        | AnsweredIncorrectly -> 0
        | DidNotAnswer -> 0

    let appealScore appealState =
        let score =
            TeamScore.initial
            |> TeamScore.failAppeal
            |> TeamScore.value

        match appealState with
        | NoFailure -> 0
        | AppealFailure -> score

    let quizzerScore questionState =
        questionState
        |> Option.map (fun (answer, appeal) -> (answerScore answer), (appealScore appeal))
        |> Option.defaultValue (0, 0)

    let scoreList questions questionNumber quizzer =
        (questions
         |> List.take (questionNumber)
         |> List.map (fun qs -> qs |> Map.tryFind (quizzer))
         |> List.map quizzerScore)

    let quizzerRunningScore questions questionNumber quizzer =
        scoreList questions questionNumber quizzer
        |> List.map fst //appeals only score at the team level
        |> List.sum

    let eventOccurred (answer, appeal) =
        match answer, appeal with
        | AnsweredCorrectly, _ -> true
        | _, AppealFailure -> true
        | AnsweredIncorrectly, NoFailure -> false
        | DidNotAnswer, NoFailure -> false

    let teamEventOccurred (team: TeamModel) question =
        team.Quizzers
        |> List.map (fun qz -> question |> Map.tryFind (qz.Name))
        |> List.exists (fun q ->
            q
            |> (Option.defaultValue (AnsweredIncorrectly, NoFailure))
            |> eventOccurred)

    let teamRunningScore questions questionNumber (team : TeamModel) =
        team.Quizzers
        |> List.fold
            (fun state qz ->
                let int32s =
                    scoreList questions questionNumber qz.Name
                    |> List.map (fun (f, s) -> f + s)

                state
                + (int32s //appeals are scored at team level
                   |> List.sum))
            0

    let findQuestionQuizzerState question (quizzer: QuizzerModel) = question |> Map.tryFind quizzer.Name

    let formatScore score =
        match score with
        | 0 -> "-"
        | number -> $"{number}"

    let showAppeal questionQuizzerState =
        match questionQuizzerState with
        | None -> "is-hidden"
        | Some (_, NoFailure) -> "is-hidden"
        | Some (_, AppealFailure) -> ""

    let quizzerView question quizzer =
        itemizedPage
            .Quizzer()
            .AppealVisible(
                quizzer
                |> findQuestionQuizzerState question
                |> showAppeal
            )
            .Score(
                quizzer
                |> findQuestionQuizzerState question
                |> quizzerScore
                |> fst
                |> formatScore
            )
            .Elt()

    itemizedPage()
        .TeamOneName(model.TeamOne.Name)
        .TeamOneColspan((model.TeamOne.Quizzers |> List.length) + 1)
        .TeamOneHeading(
            concat {
                for quizzer in model.TeamOne.Quizzers do
                    th { quizzer.Name }
            }
        )
        .TeamTwoName(model.TeamTwo.Name)
        .TeamTwoColspan((model.TeamTwo.Quizzers |> List.length) + 1)
        .TeamTwoHeading(
            concat {
                for quizzer in model.TeamTwo.Quizzers do
                    th { quizzer.Name }
            }
        )
        .Questions(
            concat {
                for (number, question) in model.Questions |> List.indexed do
                    itemizedPage
                        .Question()
                        .Number(number + 1 |> string)
                        .TeamOneScore(
                            if teamEventOccurred model.TeamOne question then
                                teamRunningScore model.Questions (number + 1) model.TeamOne
                                |> formatScore
                            else
                                "-"
                        )
                        .TeamOneQuizzers(
                            concat {
                                for quizzer in model.TeamOne.Quizzers do
                                    quizzerView question quizzer
                            }
                        )
                        .TeamTwoScore(
                            if teamEventOccurred model.TeamTwo question then
                                teamRunningScore model.Questions (number + 1) model.TeamTwo
                                |> formatScore
                            else
                                "-"
                        )
                        .TeamTwoQuizzers(
                            concat {
                                for quizzer in model.TeamTwo.Quizzers do
                                    quizzerView question quizzer
                            }
                        )
                        .Elt()
            }
        )
        .TeamOneFooter(
            concat {
                for quizzer in model.TeamOne.Quizzers do
                    td {
                        quizzerRunningScore model.Questions (model.Questions |> List.length) quizzer.Name
                        |> formatScore
                    }
            }
        )
        .TeamOneScore(model.TeamOne.Score |> string)
        .TeamTwoFooter(
            concat {
                for quizzer in model.TeamTwo.Quizzers do
                    td {
                        quizzerRunningScore model.Questions (model.Questions |> List.length) quizzer.Name
                        |> formatScore
                    }
            }
        )
        .TeamTwoScore(model.TeamTwo.Score |> string)
        .Elt()
