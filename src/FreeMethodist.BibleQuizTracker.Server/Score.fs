namespace global

open FreeMethodist.BibleQuizTracker.Server.Workflow

type AnswerState =
    | DidNotAnswer
    | AnsweredCorrectly
    | AnsweredIncorrectly

type AppealState =
    | AppealFailure
    | NoFailure

type EventState =
    { AnswerState: AnswerState
      AppealState: AppealState }

type EventPosition = QuestionNumber * Quizzer

type QuestionQuizzerEvent =
    { Position: EventPosition
      State: EventState }

type QuestionQuizzerEvents = QuestionQuizzerEvent list

type QuizzerScore =
    { AnswerScore: TeamScore
      AppealScore: TeamScore }

[<RequireQualifiedAccess>]
module Score =
    let private answerScore answerState =
        let score =
            TeamScore.zero |> TeamScore.correctAnswer

        match answerState with
        | AnsweredCorrectly -> score
        | AnsweredIncorrectly -> TeamScore.zero
        | DidNotAnswer -> TeamScore.zero

    let private appealScore appealState =
        let score =
            TeamScore.zero |> TeamScore.failAppeal

        match appealState with
        | NoFailure -> TeamScore.zero
        | AppealFailure -> score

    let quizzerScoreForTeamStyle eventState =
        eventState
        |> fun eventState -> answerScore eventState.AnswerState

    let quizzerScoreForIndividualStyle eventState =
        eventState
        |> fun eventState ->
            answerScore eventState.AnswerState
            |> TeamScore.add (appealScore eventState.AppealState)

    let teamScore eventState =
        eventState
        |> fun eventState ->
            answerScore eventState.AnswerState
            |> TeamScore.add (appealScore eventState.AppealState)

    let createScoreModelForQuestion questionNumber (questionState: QuestionState) : QuestionQuizzerEvents =
        let incorrectAnswer quizzer = (quizzer, AnsweredIncorrectly)

        let quizzerFailedAppeal failedAppeal quizzer =
            match failedAppeal with
            | None -> false
            | Some q -> q = quizzer

        let answersWithoutAppeals =
            match questionState.AnswerState with
            | Incomplete quizzers -> quizzers |> List.map incorrectAnswer
            | Complete (Answered question) ->
                [ (question.Answerer, AnsweredCorrectly) ]
                @ (question.IncorrectAnswerers
                   |> List.map incorrectAnswer)
            | Complete (Unanswered question) -> question |> List.map incorrectAnswer

        let answersWithAppealQuizzer =
            match questionState.FailedAppeal with
            | None -> answersWithoutAppeals
            | Some quizzer when
                not
                    (
                        answersWithoutAppeals
                        |> List.map fst
                        |> List.exists (fun q -> q = quizzer)
                    )
                ->
                answersWithoutAppeals
                @ [ (quizzer, DidNotAnswer) ]
            | Some _ -> answersWithoutAppeals

        let questionQuizzerEvents =
            answersWithAppealQuizzer
            |> List.map (fun (quizzer, answer) ->
                { Position = (questionNumber, quizzer)
                  State =
                    { AnswerState = answer
                      AppealState =
                        (if quizzerFailedAppeal questionState.FailedAppeal quizzer then
                             AppealFailure
                         else
                             NoFailure) } })

        questionQuizzerEvents

    let createScoreModel questions =
        questions
        |> Map.map createScoreModelForQuestion
        |> Map.toList
        |> List.collect snd

    let sumScores scores =
        scores
        |> List.ofSeq
        |> function
            | [] -> TeamScore.zero
            | scores ->
                scores
                |> Seq.reduce (fun runningScore current -> runningScore |> TeamScore.add current)
    
    let eventsForQuestion questionNumber questions =
        questions
        |> Seq.filter (fun event -> (event.Position |> fst) = questionNumber)
    
    let eventsForQuizzers quizzers questions =
        questions
        |> Seq.filter  (fun event -> quizzers |> Seq.contains (event.Position |> snd) )
    
    let scoreOfEvents scoring events =
        events
        |> Seq.map (fun event -> event.State)
        |> Seq.map scoring
        |> sumScores
    
    let calculateTeamScore quizzersOnTeam questions =
        questions
        |> eventsForQuizzers quizzersOnTeam
        |> scoreOfEvents teamScore
    
    let calculateQuizzerScore (scoringBasedOnStyle: EventState -> TeamScore) questions quizzer =
        questions
        |> eventsForQuizzers [quizzer]
        |> scoreOfEvents scoringBasedOnStyle
