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

        let answersWithoutAppeals =
            match questionState.AnswerState with
            | Incomplete quizzers -> quizzers |> List.map incorrectAnswer
            | Complete (Answered question) ->
                [ (question.Answerer, AnsweredCorrectly) ]
                @ (question.IncorrectAnswerers
                   |> List.map incorrectAnswer)
            | Complete (Unanswered question) -> question |> List.map incorrectAnswer
            |> List.map (fun (q, a) ->
                q,
                { AnswerState = a
                  AppealState = NoFailure })

        let appealsWithoutAnswers: (Quizzer * EventState) list =
            questionState.FailedAppeals
            |> List.map (fun q ->
                q,
                { AnswerState = DidNotAnswer
                  AppealState = AppealState.AppealFailure })

        let answersWithAppeals =
            answersWithoutAppeals @ appealsWithoutAnswers
            |> List.groupBy (fun e -> e |> fst)
            |> List.map (fun (quizzer, eventStates) ->
                let merged =
                    eventStates
                    |> List.map snd
                    |> List.fold
                        (fun e1 e2 ->
                            { AnswerState =
                                match e1.AnswerState, e2.AnswerState with
                                | DidNotAnswer, other -> other
                                | other, _ -> other
                              AppealState =
                                match e1.AppealState, e2.AppealState with
                                | NoFailure, other -> other
                                | other, _ -> other })
                        { AnswerState = DidNotAnswer
                          AppealState = NoFailure }

                quizzer, merged)

        let questionQuizzerEvents =
            answersWithAppeals
            |> List.map (fun (quizzer, eventState) ->
                { Position = (questionNumber, quizzer)
                  State = eventState })

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
        |> Seq.filter (fun event -> quizzers |> Seq.contains (event.Position |> snd))

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
        |> eventsForQuizzers [ quizzer ]
        |> calculate scoringBasedOnStyle

    let private updateTeamScores calculateQuizzerScore team =
        { team with
            Quizzers =
                team.Quizzers
                |> List.map (fun quizzer -> { quizzer with Score = calculateQuizzerScore quizzer.Name }) }

    let updateQuizScores (quiz: RunningQuiz) =
        match quiz.CompetitionStyle with
        | RunningCompetitionStyle.Individuals quizzerStates -> quiz
        | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
            let scoreModel =
                createScoreModel quiz.Questions
            let calcQuizzerScore = calculateQuizzerScore quizzerTeamStyleScoring scoreModel
            let updateTeamScores = updateTeamScores calcQuizzerScore
            { quiz with CompetitionStyle = RunningCompetitionStyle.Team(teamOne |> updateTeamScores, teamTwo |> updateTeamScores) }
