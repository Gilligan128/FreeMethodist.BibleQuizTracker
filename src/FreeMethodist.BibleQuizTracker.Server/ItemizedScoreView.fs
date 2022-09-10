namespace FreeMethodist.BibleQuizTracker.Server.ItemizedScoreView

open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Bolero
open Bolero.Html


module ItemizedScore =

    type private itemizedPage = Template<"wwwroot/ItemizedScore.html">

    let answerScore answerState =
        let score =
            TeamScore.zero
            |> TeamScore.correctAnswer
            |> TeamScore.value

        match answerState with
        | AnsweredCorrectly -> score
        | AnsweredIncorrectly -> 0
        | DidNotAnswer -> 0

    let appealScore appealState =
        let score =
            TeamScore.zero
            |> TeamScore.failAppeal
            |> TeamScore.value

        match appealState with
        | NoFailure -> 0
        | AppealFailure -> score

    let quizzerScore questionState =
        questionState
        |> Option.map (fun (answer, appeal) -> (answerScore answer), (appealScore appeal))
        |> Option.defaultValue (0, 0)

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

    let questionsForNumber questionNumber q = (q.Position |> fst) = questionNumber

    let questionsUpToNumber questionNumber q = (q.Position |> fst) <= questionNumber

    let questionsForQuizzer quizzerName q = (q.Position |> snd) = quizzerName

    let scoreList (questions: QuestionQuizzerEvents) questionNumber quizzer =
        questions
        |> List.filter (questionsUpToNumber questionNumber)
        |> List.filter (questionsForQuizzer quizzer)
        |> List.map (fun q -> Some(q.State.AnswerState, q.State.AppealState))
        |> List.map quizzerScore


    let quizzerRunningScore questions questionNumber quizzer =
        scoreList questions questionNumber quizzer
        |> List.map fst //appeals only score at the team level
        |> List.sum
    
    let eventHasQuizzers quizzers event = quizzers |> Seq.contains ( event.Position |> snd ) 
    
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
        |> Seq.filter (fun event -> ( event.Position |> fst ) = questionNumber)
        |> Seq.filter (eventHasQuizzers team.Quizzers)
        |> Seq.map (fun event -> event.State)
        |> Seq.map Score.teamScore
        |> Score.sumScores
        |> TeamScore.value

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
                |> Option.map (function (answer, appeal) -> {AnswerState = answer; AppealState = appeal})
                |> Option.map scoringBasedOnStyle
                |> Option.map TeamScore.value
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

    let private individualsHeader (model: Quizzer list) = empty ()

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
                <| quizzerView Score.quizzerScoreForTeamStyle questionsAdapted

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
                <| quizzerView Score.quizzerScoreForTeamStyle questionsAdapted
            }

    let individualsBody quizzers = empty ()
    
    
    let teamTotal questionQuizEvents ((teamOne: ItemizedTeam), (teamTwo: ItemizedTeam)) =
        let maxQuestion =
            questionQuizEvents
            |> List.map (fun q -> q.Position)
            |> List.map fst
            |> fun (list) -> if list.IsEmpty then PositiveNumber.one else List.max list

        let teamScoreNode (team : ItemizedTeam) : Node =
            td {
                attr.``class`` "has-text-weight-bold"
                
                questionQuizEvents
                |> Seq.filter (eventHasQuizzers team.Quizzers )
                |> Seq.map (fun event -> event.State)
                |> Seq.map Score.teamScore
                |> Score.sumScores
                |> TeamScore.value
                |> string
            }

        let quizzersTotalNode quizzers =
            forEach quizzers
            <| fun quizzer ->
                td {
                    text (
                        quizzerRunningScore questionQuizEvents maxQuestion quizzer
                        |> string
                    )
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

    let individualsTotal quizzers = empty ()

    let render (model: ItemizedScoreModel) dispatch =
        let numberOfQuestions =
            model.NumberOfQuestions

        itemizedPage()
            .Header(
                match model.CompetitionStyle with
                | ItemizedCompetitionStyle.Individual quizzers -> individualsHeader quizzers
                | ItemizedCompetitionStyle.Team (teamOne, teamTwo) -> teamHeader (teamOne, teamTwo)
            )
            .Body(
                match model.CompetitionStyle with
                | ItemizedCompetitionStyle.Individual quizzers -> individualsBody quizzers
                | ItemizedCompetitionStyle.Team (teamOne, teamTwo) ->
                    teamBody (model.QuestionsWithEvents, (numberOfQuestions |> PositiveNumber.value)) (teamOne, teamTwo)
            )           
            .Total(
                match model.CompetitionStyle with
                | ItemizedCompetitionStyle.Individual quizzers -> individualsTotal quizzers
                | ItemizedCompetitionStyle.Team (teamOne, teamTwo) ->
                    teamTotal model.QuestionsWithEvents (teamOne, teamTwo)
            )
            .Elt()
