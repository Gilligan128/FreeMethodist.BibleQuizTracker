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
    { AnswerScore: QuizScore
      AppealScore: QuizScore }

[<RequireQualifiedAccess>]
module Score =
    let private answerScore answerState =
        let score =
            QuizScore.zero |> QuizScore.correctAnswer

        match answerState with
        | AnsweredCorrectly -> score
        | AnsweredIncorrectly -> QuizScore.zero
        | DidNotAnswer -> QuizScore.zero

    let private appealScore appealState =
        let score =
            QuizScore.zero |> QuizScore.failAppeal

        match appealState with
        | NoFailure -> QuizScore.zero
        | AppealFailure -> score

    let quizzerTeamStyleScoring eventState =
        eventState
        |> fun eventState -> answerScore eventState.AnswerState

    let quizzerIndividualStyleScoring eventState =
        eventState
        |> fun eventState ->
            answerScore eventState.AnswerState
            |> QuizScore.add (appealScore eventState.AppealState)

    let teamScoring eventState =
        eventState
        |> fun eventState ->
            answerScore eventState.AnswerState
            |> QuizScore.add (appealScore eventState.AppealState)

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
            | [] -> QuizScore.zero
            | scores ->
                scores
                |> Seq.reduce (fun runningScore current -> runningScore |> QuizScore.add current)
    
    let eventsForQuestion questionNumber questions =
        questions
        |> Seq.filter (fun event -> (event.Position |> fst) = questionNumber)
    
    let eventsForQuizzers quizzers questions =
        questions
        |> Seq.filter  (fun event -> quizzers |> Seq.contains (event.Position |> snd) )
    
    let calculate scoring events =
        events
        |> Seq.map (fun event -> event.State)
        |> Seq.map scoring
        |> sumScores
    
    let calculateTeamScore quizzersOnTeam questions =
        questions
        |> eventsForQuizzers quizzersOnTeam
        |> calculate teamScoring
    
    let calculateQuizzerScore (scoringBasedOnStyle: EventState -> QuizScore) questions quizzer =
        questions
        |> eventsForQuizzers [quizzer]
        |> calculate scoringBasedOnStyle
