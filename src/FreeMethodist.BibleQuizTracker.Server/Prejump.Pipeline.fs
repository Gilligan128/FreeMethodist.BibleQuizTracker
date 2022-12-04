module FreeMethodist.BibleQuizTracker.Server.Prejump_Pipeline

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow

let updateCurrentQuestion updater quiz =
    { quiz with
        Questions =
            quiz.Questions
            |> Map.change quiz.CurrentQuestion (fun questionOpt ->
                questionOpt
                |> Option.defaultValue QuestionState.initial
                |> fun question -> question |> updater |> Some) }

let updateQuizWithCurrentQuizzerPrejump (quiz: RunningQuiz) =
    let currentQuizzerResult =
        quiz.CurrentQuizzer
        |> Result.ofOption Prejump.Error.NoCurrentQuizzer

    currentQuizzerResult
    |> Result.map (fun currentQuizzer ->
        quiz
        |> updateCurrentQuestion (fun question ->
            { question with
                Prejumps =
                    question.Prejumps @ [ currentQuizzer ]
                    |> List.distinct })
//        |> updateCurrentQuestion (fun question ->
//            match quiz.CompetitionStyle with
//            | Team(teamOne, teamTwo) -> quiz.CompetitionStyle))
