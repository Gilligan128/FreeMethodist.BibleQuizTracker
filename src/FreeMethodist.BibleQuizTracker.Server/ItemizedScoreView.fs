namespace FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView

open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Bolero
open Bolero.Html


module ItemizedScore =

    type private itemizedPage = Template<"wwwroot/ItemizedScore.html">

    let render (model: ItemizedScoreModel) dispatch =
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

        let scoreList (questions : QuestionQuizzerEvents) questionNumber quizzer =
           questions
                |> List.filter (fun q -> (q.Position |> fst) <= questionNumber )
                |> List.filter (fun q -> (q.Position |> snd) = quizzer)
                |> List.map (fun q -> Some (q.State.AnswerState, q.State.AppealState))
                |> List.map quizzerScore
       

        let quizzerRunningScore questions questionNumber quizzer =
            scoreList questions questionNumber quizzer
            |> List.map fst //appeals only score at the team level
            |> List.sum

        let eventOccurred (eventState: EventState) =
            match eventState.AnswerState, eventState.AppealState with
            | AnsweredCorrectly, _ -> true
            | _, AppealFailure -> true
            | AnsweredIncorrectly, NoFailure -> false
            | DidNotAnswer, NoFailure -> false

        let teamEventOccurred (team: TeamModel) question =
            team.Quizzers
            |> List.map (fun qz -> question |> Map.tryFind (qz.Name))
            |> List.exists (fun q ->
                q
                |> (Option.defaultValue { AnswerState = AnsweredIncorrectly;AppealState = NoFailure})
                |> eventOccurred)

        let teamRunningScore questions  questionNumber (team: TeamModel) =
            team.Quizzers
            |> List.fold
                (fun state qz ->
                    let QuizzerRunnigScoreWithAppeals =
                        scoreList questions questionNumber qz.Name
                        |> List.map (fun (f, s) -> f + s)

                    state
                    + (QuizzerRunnigScoreWithAppeals
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

        let numberOfQuestions =
            model.Questions
            |> List.map (fun q ->
                let questionNumber, _ = q.Position
                questionNumber |> PositiveNumber.value)
            |> List.max

        let quizzerView questionEvents quizzer =
            itemizedPage
                .Quizzer()
                .AppealVisible(
                    quizzer
                    |> findQuestionQuizzerState questionEvents
                    |> showAppeal
                )
                .Score(
                    quizzer
                    |> findQuestionQuizzerState questionEvents
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
                    for questionInt in 1..numberOfQuestions do
                        let questionNumber = questionInt |> PositiveNumber.numberOrOne
                        let currentQuestionEvents =
                            model.Questions
                            |> List.filter (fun q -> q.Position |> fst  = questionNumber)
                            |> List.map (fun q -> (q.Position |> snd), q.State)
                            |> Map.ofList
                            
                        let questionsAdapted = (currentQuestionEvents |> Map.map (fun k v -> v.AnswerState, v.AppealState))
                        itemizedPage
                            .Question()
                            .Number(questionNumber |> string)
                            .TeamOneScore(
                                if teamEventOccurred model.TeamOne currentQuestionEvents then
                                    teamRunningScore model.Questions questionNumber model.TeamOne
                                    |> formatScore
                                else
                                    "-"
                            )
                            .TeamOneQuizzers(
                                concat {
                                    for quizzer in model.TeamOne.Quizzers do
                                        quizzerView questionsAdapted quizzer
                                }
                            )
                            .TeamTwoScore(
                                if teamEventOccurred model.TeamTwo currentQuestionEvents then
                                    teamRunningScore model.Questions questionNumber model.TeamTwo
                                    |> formatScore
                                else
                                    "-"
                            )
                            .TeamTwoQuizzers(
                                concat {
                                    for quizzer in model.TeamTwo.Quizzers do
                                        quizzerView questionsAdapted quizzer
                                }
                            )
                            .Elt()
                }
            )
            .TeamOneFooter(
                concat {
                    for quizzer in model.TeamOne.Quizzers do
                        td {
                            quizzerRunningScore model.Questions (numberOfQuestions |> PositiveNumber.numberOrOne) quizzer.Name
                            |> formatScore
                        }
                }
            )
            .TeamOneScore(model.TeamOne.Score |> string)
            .TeamTwoFooter(
                concat {
                    for quizzer in model.TeamTwo.Quizzers do
                        td {
                            quizzerRunningScore model.Questions (numberOfQuestions |> PositiveNumber.numberOrOne) quizzer.Name
                            |> formatScore
                        }
                }
            )
            .TeamTwoScore(model.TeamTwo.Score |> string)
            .Elt()
