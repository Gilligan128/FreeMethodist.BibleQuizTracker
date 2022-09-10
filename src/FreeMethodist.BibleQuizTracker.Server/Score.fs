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
            TeamScore.initial
            |> TeamScore.correctAnswer
            |> TeamScore.value

        match answerState with
        | AnsweredCorrectly -> score
        | AnsweredIncorrectly -> 0
        | DidNotAnswer -> 0

    let private appealScore appealState =
        let score =
            TeamScore.initial
            |> TeamScore.failAppeal
            |> TeamScore.value

        match appealState with
        | NoFailure -> 0
        | AppealFailure -> score

    let quizzerScoreForTeamStyle eventState =
        eventState
        |> fun eventState -> answerScore eventState.AnswerState

    let quizzerScoreForIndividualStyle eventState =
        eventState
        |> fun eventState ->
            answerScore eventState.AnswerState
            + appealScore eventState.AppealState

    let teamScore eventState =
        eventState
        |> fun eventState ->
            answerScore eventState.AnswerState
            + appealScore eventState.AppealState

    let refreshQuestionScore questionNumber (answerState, failedAppeal) : QuestionQuizzerEvents =
        let incorrectAnswer quizzer = (quizzer, AnsweredIncorrectly)

        let quizzerFailedAppeal failedAppeal quizzer =
            match failedAppeal with
            | None -> false
            | Some q -> q = quizzer

        let answersWithoutAppeals =
            match answerState with
            | Incomplete quizzers -> quizzers |> List.map incorrectAnswer
            | Complete (Answered question) ->
                [ (question.Answerer, AnsweredCorrectly) ]
                @ (question.IncorrectAnswerers
                   |> List.map incorrectAnswer)
            | Complete (Unanswered question) -> question |> List.map incorrectAnswer

        let answersWithAppealQuizzer =
            match failedAppeal with
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
                        (if quizzerFailedAppeal failedAppeal quizzer then
                             AppealFailure
                         else
                             NoFailure) } })

        questionQuizzerEvents

    let refreshQuestionScores questions =
        questions
        |> Map.map refreshQuestionScore
        |> Map.toList
        |> List.collect snd
