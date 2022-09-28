namespace FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView

open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Bolero
open Bolero.Html


module ItemizedScore =

    type private itemizedPage = Template<"wwwroot/ItemizedScore.html">

    let findQuestionQuizzerState question quizzer = question |> Map.tryFind quizzer

    let formatScore score =
        match score with
        | 0 -> "-"
        | number -> $"{number}"

    let showAppeal questionQuizzerState =
        match questionQuizzerState with
        | None -> "is-hidden"
        | Some (_, NoFailure) -> "is-hidden"
        | Some (_, AppealFailure) -> ""

    let eventHasQuizzers quizzers event =
        quizzers |> Seq.contains (event.Position |> snd)

    let eventOccurred (eventState: EventState) =
        match eventState.AnswerState, eventState.AppealState with
        | AnsweredCorrectly, _ -> true
        | _, AppealFailure -> true
        | AnsweredIncorrectly, NoFailure -> false
        | DidNotAnswer, NoFailure -> false

    let teamEventOccurred (team: ItemizedTeam) question =
        team.Quizzers
        |> List.map (fun qz -> question |> Map.tryFind qz)
        |> List.exists (fun q ->
            q
            |> (Option.defaultValue
                    { AnswerState = AnsweredIncorrectly
                      AppealState = NoFailure })
            |> eventOccurred)

    let teamScoreForQuestion questions questionNumber (team: ItemizedTeam) =
        questions
        |> Score.eventsForQuestion questionNumber
        |> Score.eventsForQuizzers team.Quizzers
        |> Score.calculate Score.teamScoring
        |> QuizScore.value

    let quizzerView scoringBasedOnStyle questionEvents quizzer =
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
                |> Option.map (function
                    | (answer, appeal) ->
                        { AnswerState = answer
                          AppealState = appeal })
                |> Option.map scoringBasedOnStyle
                |> Option.map QuizScore.value
                |> Option.defaultValue 0
                |> formatScore
            )
            .Elt()

    let private teamHeader (teamOne: ItemizedTeam, teamTwo: ItemizedTeam) =
        let teamOneColumns =
            teamOne.Quizzers.Length + 1

        let teamTwoColumns =
            teamTwo.Quizzers.Length + 1

        concat {
            tr {
                th {
                    attr.colspan $"{teamOneColumns}"
                    $"Team {teamOne.Name}"
                }

                th { " " }

                th {
                    attr.colspan $"{teamTwoColumns}"
                    $"Team {teamTwo.Name}"
                }
            }

            tr {


                forEach teamOne.Quizzers
                <| fun quizzer -> th { quizzer }

                th { "Team Total" }
                th { "Question" }
                th { "Team Total" }

                forEach teamTwo.Quizzers
                <| fun quizzer -> th { quizzer }
            }
        }

    let private individualsHeader (model: Quizzer list) =
        concat {
            tr {
                th { "Question" }
                forEach model <| fun quizzer -> th { quizzer }

            }
        }


    let private teamBody
        (questionEvents: QuestionQuizzerEvents, numberOfQuestions)
        (teamOne: ItemizedTeam, teamTwo: ItemizedTeam)
        =


        forEach (
            [ 1..numberOfQuestions ]
            |> List.map PositiveNumber.numberOrOne
        )
        <| fun number ->
            let currentQuestionEvents =
                questionEvents
                |> List.filter (fun q -> q.Position |> fst = number)
                |> List.map (fun q -> (q.Position |> snd), q.State)
                |> Map.ofList

            let questionsAdapted =
                (currentQuestionEvents
                 |> Map.map (fun k v -> v.AnswerState, v.AppealState))

            tr {

                forEach teamOne.Quizzers
                <| quizzerView Score.quizzerTeamStyleScoring questionsAdapted

                td {
                    if teamEventOccurred teamOne currentQuestionEvents then
                        teamScoreForQuestion questionEvents number teamOne
                        |> formatScore
                    else
                        "-"
                }

                td { text (number |> PositiveNumber.value |> string) }

                td {
                    if teamEventOccurred teamTwo currentQuestionEvents then
                        teamScoreForQuestion questionEvents number teamTwo
                        |> formatScore
                    else
                        "-"
                }

                forEach teamTwo.Quizzers
                <| quizzerView Score.quizzerTeamStyleScoring questionsAdapted
            }

    let individualsBody (questionEvents: QuestionQuizzerEvents, numberOfQuestions) quizzers =
        forEach (
            [ 1..numberOfQuestions ]
            |> List.map PositiveNumber.numberOrOne
        )
        <| fun number ->
            let currentQuestionEvents =
                questionEvents
                |> List.filter (fun q -> q.Position |> fst = number)
                |> List.map (fun q -> (q.Position |> snd), q.State)
                |> Map.ofList

            let questionsAdapted =
                (currentQuestionEvents
                 |> Map.map (fun k v -> v.AnswerState, v.AppealState))

            tr {
                td { text (number |> PositiveNumber.value |> string) }

                forEach quizzers
                <| quizzerView Score.quizzerIndividualStyleScoring questionsAdapted

            }


    let private quizzersTotalNode calculateTotalScore quizzers =
        forEach quizzers
        <| fun quizzer ->
            td {
                text (
                    quizzer
                    |> calculateTotalScore
                    |> QuizScore.value
                    |> string
                )
            }

    let teamTotal
        (quizzersTotalNode: Quizzer list -> Node)
        questionQuizEvents
        ((teamOne: ItemizedTeam), (teamTwo: ItemizedTeam))
        =

        let teamScoreNode (team: ItemizedTeam) : Node =
            td {
                attr.``class`` "has-text-weight-bold"

                questionQuizEvents
                |> Score.calculateTeamScore team.Quizzers
                |> QuizScore.value
                |> string
            }


        tr {
            quizzersTotalNode teamOne.Quizzers
            teamScoreNode teamOne

            td {
                attr.``class`` "has-text-weight-bold"
                "TOTAL"
            }

            teamScoreNode teamTwo
            quizzersTotalNode teamTwo.Quizzers
        }

    let individualsTotal (quizzersTotalNode: Quizzer list -> Node) quizzers =
        tr {
            td {
                attr.``class`` "has-text-weight-bold"
                "TOTAL"
            }

            quizzersTotalNode quizzers
        }

    let render (model: ItemizedScoreModel) dispatch =
        let numberOfQuestions =
            model.NumberOfQuestions
            |> PositiveNumber.value

        let calculateScore =
            Score.calculateQuizzerScore Score.quizzerIndividualStyleScoring model.QuestionsWithEvents

        let quizzersTotalNode =
            quizzersTotalNode calculateScore
        
        
        
        itemizedPage()
            .Header(
                match model.CompetitionStyle with
                | ItemizedCompetitionStyle.Individual quizzers -> individualsHeader quizzers
                | ItemizedCompetitionStyle.Team (teamOne, teamTwo) -> teamHeader (teamOne, teamTwo)
            )
            .Body(
                match model.CompetitionStyle with
                | ItemizedCompetitionStyle.Individual quizzers -> individualsBody (model.QuestionsWithEvents, numberOfQuestions) quizzers
                | ItemizedCompetitionStyle.Team (teamOne, teamTwo) ->
                    teamBody (model.QuestionsWithEvents, numberOfQuestions) (teamOne, teamTwo)
            )
            .Total(
                match model.CompetitionStyle with
                | ItemizedCompetitionStyle.Individual quizzers -> individualsTotal quizzersTotalNode quizzers
                | ItemizedCompetitionStyle.Team (teamOne, teamTwo) ->
                    teamTotal quizzersTotalNode model.QuestionsWithEvents (teamOne, teamTwo)
            )
            .Elt()
