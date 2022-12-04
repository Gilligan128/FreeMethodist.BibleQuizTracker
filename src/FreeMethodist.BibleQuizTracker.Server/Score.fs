namespace global

open FreeMethodist.BibleQuizTracker.Server.Workflow

type AnswerState =
    | DidNotAnswer
    | AnsweredCorrectly
    | AnsweredIncorrectly

type AppealState =
    | AppealFailure
    | NoFailure

type PrejumpState =
    | NormalJump of int
    | Prejump

type EventState =
    { AnswerState: AnswerState
      AppealState: AppealState
      JumpState: Option<PrejumpState> }

type EventPosition = QuestionNumber * Quizzer

type QuestionQuizzerEvent =
    { Position: EventPosition
      State: EventState }

type QuestionQuizzerEvents = QuestionQuizzerEvent list

type AccumulatedEventState =
    { NumberOfMisPrejumps: int
      Events: EventState }

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

    let private misprejumpScore (numberOfMisPrejumps, jumpState, answerState) =
        match jumpState, answerState with
        | (Some Prejump), AnsweredIncorrectly when numberOfMisPrejumps >= 4 -> QuizScore.misPrejump
        | _, _ -> QuizScore.zero

    let quizzerTeamStyleScoringOld eventState =
        eventState
        |> fun eventState -> answerScore eventState.AnswerState

    let quizzerIndividualStyleScoringOld eventState =
        eventState
        |> fun eventState ->
            answerScore eventState.AnswerState
            |> QuizScore.add (appealScore eventState.AppealState)

    let teamScoringOld eventState =
        eventState
        |> fun eventState ->
            answerScore eventState.AnswerState
            |> QuizScore.add (appealScore eventState.AppealState)

    let quizzerTeamStyleScoring eventState =
        eventState
        |> fun eventState -> answerScore eventState.Events.AnswerState

    let quizzerIndividualStyleScoring eventState =
        eventState
        |> fun eventState ->
            answerScore eventState.Events.AnswerState
            |> QuizScore.add (appealScore eventState.Events.AppealState)
            |> QuizScore.add (
                misprejumpScore (
                    eventState.NumberOfMisPrejumps,
                    eventState.Events.JumpState,
                    eventState.Events.AnswerState
                )
            )

    let createScoreModelForQuestion questionNumber (questionState: QuestionState) : QuestionQuizzerEvents =
        let correctQuizzerOpt, incorrectQuizzers =
            match questionState.AnswerState with
            | QuizAnswer.Complete (CompletedAnswer.Answered answeredQuestion) ->
                (Some answeredQuestion.Answerer), answeredQuestion.IncorrectAnswerers
            | QuizAnswer.Complete (CompletedAnswer.Unanswered incorrectQuizzers) -> None, incorrectQuizzers
            | QuizAnswer.Incomplete quizzers -> None, quizzers

        let quizzers =
            questionState.Prejumps
            @ questionState.FailedAppeals
              @ (correctQuizzerOpt |> Option.toList)
                @ incorrectQuizzers
            |> List.distinct

        let questionQuizzerEvents =
            quizzers
            |> List.map (fun quizzer ->
                { Position = (questionNumber, quizzer)
                  State =
                    { AnswerState =
                        if correctQuizzerOpt |> Option.defaultValue "" = quizzer then
                            AnsweredCorrectly
                        else if incorrectQuizzers |> Seq.contains quizzer then
                            AnsweredIncorrectly
                        else
                            DidNotAnswer
                      AppealState =
                        if questionState.FailedAppeals
                           |> List.contains quizzer then
                            AppealFailure
                        else
                            NoFailure
                      JumpState =
                        if questionState.Prejumps |> List.contains quizzer then
                            Some Prejump
                        else
                            None } })

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

    let private misPrejumptIncrease event =
        event.JumpState
        |> Option.map (fun j ->
            match j, event.AnswerState with
            | Prejump, AnsweredIncorrectly -> 1
            | _, _ -> 0)
        |> Option.defaultValue 0

    let calculateWithmisprejumps scoring events =
        let scannedEvents =
            events
            |> Seq.map (fun event -> event.State)
            |> Seq.scan
                (fun state event ->
                    { NumberOfMisPrejumps =
                        event
                        |> misPrejumptIncrease
                        |> (fun increase -> increase + state.NumberOfMisPrejumps)
                      Events = event })
                { NumberOfMisPrejumps = 0
                  Events =
                    { AnswerState = DidNotAnswer
                      AppealState = NoFailure
                      JumpState = None } }
            |> Seq.map scoring
            |> sumScores

        scannedEvents

    let calculateTeamScore quizzersOnTeam questions =
        questions
        |> eventsForQuizzers quizzersOnTeam
        |> calculate teamScoringOld

    let calculateQuizzerScore (scoringBasedOnStyle: EventState -> QuizScore) questions quizzer =
        questions
        |> eventsForQuizzers [ quizzer ]
        |> calculate scoringBasedOnStyle

    let private updateQuizzerScores calculateQuizzerScore quizzers : QuizzerState list =
        quizzers
        |> List.map (fun quizzer -> { quizzer with Score = calculateQuizzerScore quizzer.Name })

    let private updateTeamScores calculateQuizzerScore calculateTeamScore team =
        { team with
            Score =
                team.Quizzers
                |> List.map (fun quizzer -> quizzer.Name)
                |> calculateTeamScore
            Quizzers = updateQuizzerScores calculateQuizzerScore team.Quizzers }

    let updateQuizScores (quiz: RunningQuiz) =
        let scoreModel =
            createScoreModel quiz.Questions

        match quiz.CompetitionStyle with
        | RunningCompetitionStyle.Individuals quizzerStates ->
            let calcQuizzerScore =
                calculateQuizzerScore quizzerIndividualStyleScoringOld scoreModel

            { quiz with
                CompetitionStyle =
                    RunningCompetitionStyle.Individuals(updateQuizzerScores calcQuizzerScore quizzerStates) }
        | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
            let calcQuizzerScore =
                calculateQuizzerScore quizzerTeamStyleScoringOld scoreModel

            let calcTeamScore quizzers =
                scoreModel |> calculateTeamScore quizzers

            let updateTeamScores =
                updateTeamScores calcQuizzerScore calcTeamScore

            { quiz with
                CompetitionStyle =
                    RunningCompetitionStyle.Team(teamOne |> updateTeamScores, teamTwo |> updateTeamScores) }
