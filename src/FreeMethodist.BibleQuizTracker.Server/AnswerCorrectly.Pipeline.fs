module FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core

//Answer Question
type AnswerQuestion = Quizzer -> AnsweredQuestion

type UpdatedQuiz =
    { QuizState: RunningTeamQuiz
      RevertedAnswer: RevertedCorrectAnswer }

type UpdateQuiz = Quizzer -> RunningTeamQuiz -> Result<UpdatedQuiz, AnswerCorrectly.Error>

type CreateEvents = Quizzer -> UpdatedQuiz -> AnswerCorrectly.Event list

let private updateQuizzerScore changeScore (quizzer: QuizzerState) =
    { quizzer with Score = quizzer.Score |> changeScore }

let private updateAnsweringQuizzer changeScore quizzerName quizzers =
    quizzers
    |> List.map (fun q ->
        if QuizzerState.isQuizzer quizzerName q then
            (updateQuizzerScore changeScore q)
        else
            q)

let private recordAnsweredQuestion quizzer currentQuestion (initialQuestionState) =
    initialQuestionState
    |> QuizAnswer.answerCorrectly quizzer currentQuestion


let revertTeamScoreIfQuizzerOnTeam quizzer (team: QuizTeamState) =
    let quizzerFound =
        team.Quizzers
        |> List.exists (QuizzerState.isQuizzer quizzer)

    if quizzerFound then
        { team with Score = team.Score |> TeamScore.revertCorrectAnswer }
    else
        team

let possiblyRevertTeamAndIndividualScores revertQuizzerScore reverted team =
    team
    |> QuizTeamState.updateQuizzerIfFound revertQuizzerScore reverted
    |> revertTeamScoreIfQuizzerOnTeam reverted

let possiblyRevertQuizScores revertedAnswer (quiz: RunningTeamQuiz) =
    let revertQuizzerScore q : QuizzerState =
        { q with Score = q.Score |> TeamScore.revertCorrectAnswer }

    { quiz with
        TeamOne =
            quiz.TeamOne
            |> possiblyRevertTeamAndIndividualScores revertQuizzerScore revertedAnswer
        TeamTwo =
            quiz.TeamTwo
            |> possiblyRevertTeamAndIndividualScores revertQuizzerScore revertedAnswer }

let updateQuizLevelInfo quizzer (quiz: RunningTeamQuiz) =
    result {
        let newCurrentQuestion =
            quiz.CurrentQuestion |> PositiveNumber.increment

        let! updatedAnswer, revertedQuizzer =
            quiz.Questions
            |> Map.tryFind quiz.CurrentQuestion
            |> Option.map (fun q -> q.AnswerState)
            |> recordAnsweredQuestion quizzer quiz.CurrentQuestion
            |> Result.mapError (fun error ->
                error
                |> AnswerCorrectly.Error.QuizzerAlreadyAnsweredCorrectly)

        let currentQuestionInQuiz =
            { quiz with Questions = RunningTeamQuiz.changeCurrentAnswer quiz updatedAnswer }
            |> changeCurrentQuestionInQuiz newCurrentQuestion

        return currentQuestionInQuiz, revertedQuizzer
    }

let updateTeam changeScore quizzerName (team: QuizTeamState) =
    { team with
        Quizzers =
            team.Quizzers
            |> updateAnsweringQuizzer changeScore quizzerName
        Score = team.Score |> changeScore }

let updateTeamAndQuizzerScore changeScore quiz (teamOne, teamTwo) quizzerName =
    let updateTeam =
        updateTeam changeScore quizzerName

    quizzerName
    |> RunningTeamQuiz.tryFindQuizzerAndTeam (teamOne, teamTwo)
    |> Option.map (function
        | _, TeamOne ->
            { quiz with
                TeamOne = updateTeam teamOne
                CompetitionStyle = RunningCompetitionStyle.Team(updateTeam teamOne, teamTwo) }
        | _, TeamTwo ->
            { quiz with
                TeamTwo = updateTeam teamTwo
                CompetitionStyle = RunningCompetitionStyle.Team(teamOne, teamTwo) })

let updateIndividualQuizzerScore changeScore quizzerName (updatedQuizInfo: RunningTeamQuiz) quizzerStates =
    let quizzerExistsResult =
        if
            quizzerStates
            |> List.exists (QuizzerState.isQuizzer quizzerName)
        then
            Some quizzerStates
        else
            None

    quizzerExistsResult
    |> Option.map (fun quizzerStates ->
        { updatedQuizInfo with
            CompetitionStyle =
                quizzerStates
                |> updateAnsweringQuizzer changeScore quizzerName
                |> RunningCompetitionStyle.Individuals })

let updateScoresBasedOnQuizzer changeScore quizzerName (updatedQuizInfo: RunningTeamQuiz) =
    match updatedQuizInfo.CompetitionStyle with
    | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
        updateTeamAndQuizzerScore
            changeScore
            updatedQuizInfo
            (updatedQuizInfo.TeamOne, updatedQuizInfo.TeamTwo)
            quizzerName
    | RunningCompetitionStyle.Individuals quizzerStates ->
        updateIndividualQuizzerScore changeScore quizzerName updatedQuizInfo quizzerStates

let updateQuiz: UpdateQuiz =
    fun quizzerName quiz ->
        result {

            let! updatedQuizInfo, revertedAnswer = updateQuizLevelInfo quizzerName quiz

            let revertedOpt =
                match revertedAnswer with
                | NoChange -> None
                | Reverted reverted -> Some reverted

            let! updatedQuizWithScores =
                updateScoresBasedOnQuizzer TeamScore.correctAnswer quizzerName updatedQuizInfo
                |> Result.ofOption (AnswerCorrectly.QuizzerNotFound quizzerName)

            return
                revertedOpt
                |> Option.bind (fun reverted ->
                    updateScoresBasedOnQuizzer TeamScore.revertCorrectAnswer reverted updatedQuizWithScores)
                |> Option.defaultValue updatedQuizWithScores
                |> fun quiz ->
                    { QuizState = quiz
                      RevertedAnswer = revertedAnswer }
        }


let createEvents: CreateEvents =
    fun quizzer updatedQuiz ->
        let quizState = updatedQuiz.QuizState

        let updatedTeamScore (quiz: RunningTeamQuiz) teamPosition =
            match teamPosition with
            | TeamOne -> quiz.TeamOne.Score
            | TeamTwo -> quiz.TeamTwo.Score

        let answerer, teamUpdatedOpt =
            RunningTeamQuiz.findQuizzer quizState quizzer

        let answererScoreChanged =
            AnswerCorrectly.Event.IndividualScoreChanged
                { NewScore = answerer.Score
                  Quiz = quizState.Code
                  Quizzer = quizzer
                  Question = quizState.CurrentQuestion }

        let revertedScoresChanged =
            match updatedQuiz.RevertedAnswer with
            | NoChange -> []
            | Reverted revertedQuizzer ->
                let revertedState, revertedTeamOpt =
                    revertedQuizzer
                    |> RunningTeamQuiz.findQuizzer quizState

                let scoreChanged =
                    { NewScore = revertedState.Score
                      Quiz = quizState.Code
                      Quizzer = revertedQuizzer
                      Question = quizState.CurrentQuestion }
                    |> AnswerCorrectly.Event.IndividualScoreChanged

                let teamScoreChanged =
                    Option.map2
                        (fun teamUpdated teamReverted ->
                            if teamUpdated <> teamReverted then
                                [ AnswerCorrectly.Event.TeamScoreChanged
                                      { NewScore = updatedTeamScore quizState teamReverted
                                        Team = teamReverted
                                        Quiz = quizState.Code } ]
                            else
                                [])
                        teamUpdatedOpt
                        revertedTeamOpt
                    |> Option.defaultValue []

                [ scoreChanged
                  yield! teamScoreChanged ]

        let teamScoreChanged =
            teamUpdatedOpt
            |> Option.map (fun teamUpdated ->
                [ AnswerCorrectly.Event.TeamScoreChanged
                      { NewScore = updatedTeamScore quizState teamUpdated
                        Team = teamUpdated
                        Quiz = quizState.Code } ])
            |> Option.defaultValue []


        let currentQuestionChanged =
            AnswerCorrectly.Event.CurrentQuestionChanged
                { Quiz = quizState.Code
                  NewQuestion = quizState.CurrentQuestion }

        [ yield! teamScoreChanged
          answererScoreChanged
          currentQuestionChanged
          yield! revertedScoresChanged ]

let answerCorrectly getQuiz saveQuiz : AnswerCorrectly.Workflow =
    fun command ->
        asyncResult {
            let! quiz =
                getQuiz command.Quiz
                |> AsyncResult.mapError AnswerCorrectly.Error.DbError
                |> AsyncResult.bind (fun quiz ->
                    quiz
                    |> Common.Pipeline.validateRunningQuiz
                    |> AsyncResult.ofResult
                    |> AsyncResult.mapError AnswerCorrectly.Error.QuizStateError)

            let! currentQuizzer =
                quiz
                |> validateCurrentQuizzer
                |> Result.mapError (fun e -> AnswerCorrectly.Error.NoCurrentQuizzer)
                |> AsyncResult.ofResult

            let! updatedQuiz =
                updateQuiz currentQuizzer quiz
                |> AsyncResult.ofResult

            do!
                updatedQuiz.QuizState
                |> Quiz.Running
                |> saveQuiz
                |> AsyncResult.mapError AnswerCorrectly.Error.DbError

            return createEvents currentQuizzer updatedQuiz
        }
