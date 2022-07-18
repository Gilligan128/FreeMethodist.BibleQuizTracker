module FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline

open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow.AnswerCorrectly
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.FSharp.Core

//Answer Question
type AnswerQuestion = Quizzer -> AnsweredQuestion


type UpdateQuiz = Quizzer -> RunningTeamQuiz -> Result<RunningTeamQuiz, AnswerCorrectly.Error>

type CreateEvents = Quizzer -> RunningTeamQuiz -> AnswerCorrectly.Event list

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

    let recordAnsweredQuestion quizzer (initialQuestionState) =
        let CompletedAnswered = Answered >> Complete

        let withoutAnswerer =
            List.except [ quizzer ]

        match initialQuestionState with
        | Some (Complete (Answered questionState)) ->
            CompletedAnswered
                { Answerer = quizzer
                  IncorrectAnswerers =
                    questionState.IncorrectAnswerers
                    |> withoutAnswerer }
        | Some (Complete (Unanswered questionState)) ->
            CompletedAnswered
                { Answerer = quizzer
                  IncorrectAnswerers = questionState |> withoutAnswerer }
        | Some (Incomplete answerers) ->
            CompletedAnswered
                { Answerer = quizzer
                  IncorrectAnswerers = answerers }
        | None ->
            CompletedAnswered
                { Answerer = quizzer
                  IncorrectAnswerers = [] }

    let updateQuizLevelInfo quizzer (quiz: RunningTeamQuiz) =
        let newCurrentQuestion =
            quiz.CurrentQuestion |> PositiveNumber.increment

        let updatedQuestion =
            quiz.Questions
            |> Map.tryFind newCurrentQuestion
            |> recordAnsweredQuestion quizzer

        { quiz with
            CurrentQuestion = newCurrentQuestion
            Questions =
                quiz.Questions
                |> Map.add newCurrentQuestion updatedQuestion }

    let updateTeamOpt isQuizzer team =
        team.Quizzers
        |> List.exists isQuizzer
        |> fun found -> if found then Some team else None
        |> Option.map (fun team -> { team with Quizzers = team.Quizzers |> updateAnsweringQuizzer isQuizzer })
        |> Option.map (fun team -> { team with Score = team.Score |> TeamScore.correctAnswer })

    fun quizzerName quiz ->
        let isQuizzer (quizzer: QuizzerState) = quizzer.Name = quizzerName
        let updateTeamOpt = updateTeamOpt isQuizzer

        let teamOneOpt =
            (updateTeamOpt quiz.TeamOne)

        let teamTwoOpt =
            (updateTeamOpt quiz.TeamTwo)

        let updatedQuizInfo =
            updateQuizLevelInfo quizzerName quiz

        match teamOneOpt, teamTwoOpt with
        | Some _, Some _ -> Error(DuplicateQuizzer quizzerName)
        | None, None -> Error(QuizzerNotFound quizzerName)
        | Some teamOne, None -> Ok { updatedQuizInfo with TeamOne = teamOne }
        | None, Some teamTwo -> Ok { updatedQuizInfo with TeamTwo = teamTwo }


let createEvents: CreateEvents =
    fun quizzer quizState ->
        let updatedTeamScore (quiz: RunningTeamQuiz) teamPosition =
            match teamPosition with
            | TeamOne -> quiz.TeamOne.Score
            | TeamTwo -> quiz.TeamTwo.Score

        let findAnswerer (quiz: RunningTeamQuiz) =
            [ yield!
                  (quiz.TeamOne.Quizzers
                   |> List.map (fun q -> (q, TeamOne)))
              yield!
                  (quiz.TeamTwo.Quizzers
                   |> List.map (fun q -> (q, TeamTwo))) ]
            |> List.find (fun (q, _) -> q.Name = quizzer)

        let answerer, teamUpdated =
            findAnswerer quizState

        let teamScoreChanged =
            Event.TeamScoreChanged
                { NewScore = updatedTeamScore quizState teamUpdated
                  Team = teamUpdated
                  Quiz = quizState.Code }

        let individualScoreChanged =
            Event.IndividualScoreChanged
                { NewScore = answerer.Score
                  Quiz = quizState.Code
                  Quizzer = quizzer
                  Question = quizState.CurrentQuestion }

        let currentQuestionChanged =
            Event.CurrentQuestionChanged
                { Quiz = quizState.Code
                  NewQuestion = quizState.CurrentQuestion }

        [ teamScoreChanged
          individualScoreChanged
          currentQuestionChanged ]

let answerCorrectly getQuiz saveQuiz : Workflow =
    fun command ->
        asyncResult {
            let! quiz =
                getQuiz command.Quiz
                |> AsyncResult.ofAsync
                |> AsyncResult.bind (fun quiz ->
                    quiz
                    |> Common.Pipeline.validateQuiz
                    |> AsyncResult.ofResult
                    |> AsyncResult.mapError Error.QuizStateError)
            let! updatedQuiz = updateQuiz command.Data.Quizzer quiz  |> AsyncResult.ofResult
            do! updatedQuiz |> saveQuiz |> AsyncResult.ofAsync
            return createEvents command.Data.Quizzer updatedQuiz
        }
