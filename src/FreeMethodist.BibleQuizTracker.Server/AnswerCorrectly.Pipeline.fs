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

let updateQuiz: UpdateQuiz =
    let updateQuizzerScore (quizzer: QuizzerState) =
        { quizzer with Score = quizzer.Score |> TeamScore.correctAnswer }

    let updateAnsweringQuizzer isQuizzer quizzers =
        quizzers
        |> List.map (fun q ->
            if isQuizzer q then
                (updateQuizzerScore q)
            else
                q)

    let recordAnsweredQuestion quizzer currentQuestion (initialQuestionState) =
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

    let possiblyRevertTeamAndIndividualScores revertedQuizzer team =
        let revertQuizzerScore q : QuizzerState =
            { q with Score = q.Score |> TeamScore.revertCorrectAnswer }

        match revertedQuizzer with
        | NoChange -> team
        | Reverted reverted ->
            team
            |> QuizTeamState.updateQuizzerIfFound revertQuizzerScore reverted
            |> revertTeamScoreIfQuizzerOnTeam reverted

    let possiblyRevertQuizScores revertedAnswer (quiz: RunningTeamQuiz) =
        { quiz with
            TeamOne =
                quiz.TeamOne
                |> possiblyRevertTeamAndIndividualScores revertedAnswer
            TeamTwo =
                quiz.TeamTwo
                |> possiblyRevertTeamAndIndividualScores revertedAnswer }

    let updateQuizLevelInfo quizzer (quiz: RunningTeamQuiz) =
        result {
            let newCurrentQuestion =
                quiz.CurrentQuestion |> PositiveNumber.increment

            let! updatedAnswer, revertedQuizzer =
                quiz.Questions
                |> Map.tryFind quiz.CurrentQuestion
                |> Option.map (fun q -> q.AnswerState)
                |> recordAnsweredQuestion quizzer quiz.CurrentQuestion
                |> Result.mapError (fun error -> error |> AnswerCorrectly.Error.QuizzerAlreadyAnsweredCorrectly)

            let currentQuestionInQuiz = 
                { quiz with
                    Questions = RunningTeamQuiz.changeCurrentAnswer quiz updatedAnswer}
                |> changeCurrentQuestionInQuiz newCurrentQuestion
            return
                currentQuestionInQuiz,
                revertedQuizzer
        }

    let updateTeamOpt isQuizzer team =
        team.Quizzers
        |> List.exists isQuizzer
        |> fun found -> if found then Some team else None
        |> Option.map (fun team -> { team with Quizzers = team.Quizzers |> updateAnsweringQuizzer isQuizzer })
        |> Option.map (fun team -> { team with Score = team.Score |> TeamScore.correctAnswer })

    fun quizzerName quiz ->
        result {
            let isQuizzer (quizzer: QuizzerState) = quizzer.Name = quizzerName
            let updateTeamOpt = updateTeamOpt isQuizzer

            let teamOneOpt =
                (updateTeamOpt quiz.TeamOne)

            let teamTwoOpt =
                (updateTeamOpt quiz.TeamTwo)

            let! updatedQuizInfo, revertedAnswer = updateQuizLevelInfo quizzerName quiz

            let! updatedQuizResult =
                match teamOneOpt, teamTwoOpt with
                | Some _, Some _ -> Error(AnswerCorrectly.DuplicateQuizzer quizzerName)
                | None, None -> Error(AnswerCorrectly.QuizzerNotFound quizzerName)
                | Some teamOne, None -> Ok({ updatedQuizInfo with TeamOne = teamOne })
                | None, Some teamTwo -> Ok({ updatedQuizInfo with TeamTwo = teamTwo })

            return
                updatedQuizResult
                |> possiblyRevertQuizScores revertedAnswer
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

        let findAnswerer quizzer (quiz: RunningTeamQuiz) =
            [ yield!
                  (quiz.TeamOne.Quizzers
                   |> List.map (fun q -> (q, TeamOne)))
              yield!
                  (quiz.TeamTwo.Quizzers
                   |> List.map (fun q -> (q, TeamTwo))) ]
            |> List.find (fun (q, _) -> q.Name = quizzer)

        let answerer, teamUpdated =
            findAnswerer quizzer quizState

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
                let revertedState, revertedTeam =
                    quizState |> findAnswerer revertedQuizzer

                let scoreChanged =
                    { NewScore = revertedState.Score
                      Quiz = quizState.Code
                      Quizzer = revertedQuizzer
                      Question = quizState.CurrentQuestion }
                    |> AnswerCorrectly.Event.IndividualScoreChanged

                let teamScoreChanged =
                    if teamUpdated <> revertedTeam then
                        [ AnswerCorrectly.Event.TeamScoreChanged
                              { NewScore = updatedTeamScore quizState revertedTeam
                                Team = revertedTeam
                                Quiz = quizState.Code } ]
                    else
                        []

                [ scoreChanged
                  yield! teamScoreChanged ]

        let teamScoreChanged =
            AnswerCorrectly.Event.TeamScoreChanged
                { NewScore = updatedTeamScore quizState teamUpdated
                  Team = teamUpdated
                  Quiz = quizState.Code }

        let currentQuestionChanged =
            AnswerCorrectly.Event.CurrentQuestionChanged
                { Quiz = quizState.Code
                  NewQuestion = quizState.CurrentQuestion }

        [ teamScoreChanged
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
