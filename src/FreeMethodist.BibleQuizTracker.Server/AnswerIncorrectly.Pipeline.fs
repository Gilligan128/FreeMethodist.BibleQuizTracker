module FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Pipeline

open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow
open FreeMethodist.BibleQuizTracker.Server.AnswerIncorrectly.Workflow.AnswerIncorrectly
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

type ValidCurrentQuizzer = private ValidCurrentQuizzer of Quizzer
type ValidateCurrentQuizzer = RunningTeamQuiz -> Result<ValidCurrentQuizzer, AnswerIncorrectly.Error>
type UpdateQuiz = ValidCurrentQuizzer -> RunningTeamQuiz -> RunningTeamQuiz
type CreateEvents = RunningTeamQuiz -> AnswerIncorrectly.Event list

type RevertedCorrectAnswer =
    | Reverted
    | NoChange

let validateQuizzer: ValidateCurrentQuizzer =
    fun quiz ->
        quiz.CurrentQuizzer
        |> Option.map ValidCurrentQuizzer
        |> (Result.ofOption Error.NoCurrentQuizzer)

let updateQuiz: UpdateQuiz =
    let addQuizzerDistinct quizzer quizzers = quizzers @ [ quizzer ] |> List.distinct

    let updateOrAddQuestion quizzer questionOpt =
        match questionOpt with
        | None -> Incomplete [ quizzer ], NoChange
        | Some (Incomplete quizzers) -> Incomplete(quizzers |> addQuizzerDistinct quizzer), NoChange
        | Some (Complete (Answered answeredQuestion)) when quizzer = answeredQuestion.Answerer ->
            answeredQuestion.IncorrectAnswerers
            |> addQuizzerDistinct quizzer
            |> Unanswered
            |> Complete,
            Reverted
        | Some (Complete (Answered answeredQuestion)) ->
            { answeredQuestion with
                IncorrectAnswerers =
                    answeredQuestion.IncorrectAnswerers
                    |> addQuizzerDistinct quizzer }
            |> Answered
            |> Complete,
            NoChange
        | Some (Complete (Unanswered question)) ->
            question
            |> addQuizzerDistinct quizzer
            |> (Unanswered >> Complete),
            NoChange

    fun (ValidCurrentQuizzer quizzer) quiz ->
        let quizCurrentQuestion =
            quiz.CurrentQuestion

        let currentQuestionRecord =
            quiz.Questions.TryFind quizCurrentQuestion

        let changedQuestion, revertedCorrectAnswer =
            updateOrAddQuestion quizzer currentQuestionRecord

        let updateScore revertedAnswer (quizzer: QuizzerState) =
            match revertedAnswer with
            | Reverted -> quizzer.Score |> TeamScore.revertCorrectAnswer
            | NoChange -> quizzer.Score

        let updateQuizzerWithScore revertedAnswer (quizzer: QuizzerState) =
            { quizzer with Score = updateScore revertedAnswer quizzer }

        let updateQuizzerInTeamIfFound quizzer (team: QuizTeamState) =
            { team with
                Quizzers =
                    team.Quizzers
                    |> List.map (fun q ->
                        if q.Name = quizzer then
                            (updateQuizzerWithScore revertedCorrectAnswer) q
                        else
                            q) }

        { quiz with
            CurrentQuizzer = None
            TeamOne = updateQuizzerInTeamIfFound quizzer quiz.TeamOne
            TeamTwo = updateQuizzerInTeamIfFound quizzer quiz.TeamTwo
            Questions =
                quiz.Questions
                |> Map.add quizCurrentQuestion changedQuestion }

let createEvents: CreateEvents =
    fun quiz ->
        let quizzerChanged =
            { Quiz = quiz.Code
              CurrentQuizzer = quiz.CurrentQuizzer }
            |> Event.CurrentQuizzerChanged

        [ quizzerChanged ]

let answerIncorrectly getQuiz saveQuiz : Workflow =
    fun command ->
        asyncResult {
            let! quiz = getQuiz command.Quiz |> AsyncResult.ofAsync

            let! runningQuiz =
                validateQuiz quiz
                |> AsyncResult.ofResult
                |> AsyncResult.mapError QuizState

            let! validQuizzer =
                validateQuizzer runningQuiz
                |> AsyncResult.ofResult

            let updatedQuiz =
                updateQuiz validQuizzer runningQuiz

            do!
                updatedQuiz
                |> Running
                |> saveQuiz
                |> AsyncResult.ofAsync

            return createEvents updatedQuiz
        }
