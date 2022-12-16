module FreeMethodist.BibleQuizTracker.Server.Prejump_Pipeline

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.ScoreEvents
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline


let private updateCurrentQuestion updater quiz =
    { quiz with
        Questions =
            quiz.Questions
            |> Map.change quiz.CurrentQuestion (fun questionOpt ->
                questionOpt
                |> Option.defaultValue QuestionState.initial
                |> fun question -> question |> updater |> Some) }

let private removeTeammatePrejumps quiz currentQuizzer (question : QuestionState ) =
    let teamPosition =
        quiz
        |> RunningQuiz.tryFindQuizzer2 currentQuizzer
        |> Option.bind snd

    let teammatesOpt =
        teamPosition
        |> Option.map (fun teamPosition ->
            quiz
            |> RunningQuiz.getTeam teamPosition
            |> fun team -> team.Quizzers |> List.map (fun q -> q.Name))

    teammatesOpt
    |> Option.map (fun teammates -> { question with Prejumps = question.Prejumps |> List.except teammates })
    |> Option.defaultValue question

let updateQuizWithCurrentQuizzerPrejump (quiz: RunningQuiz) =
    let currentQuizzerResult =
        quiz.CurrentQuizzer
        |> Result.ofOption (Prejump.Error.NoCurrentQuizzer NoCurrentQuizzer)

    currentQuizzerResult
    |> Result.map (fun currentQuizzer ->
        quiz
        |> updateCurrentQuestion (removeTeammatePrejumps quiz currentQuizzer)
        |> updateCurrentQuestion (fun question ->
            { question with
                Prejumps =
                    question.Prejumps @ [ currentQuizzer ]
                    |> List.distinct }))

let prejumpWorkflow quiz _ =
    result {
        let! quiz = validateRunningQuiz quiz
                    |> Result.mapError Prejump.Error.WrongQuizState
        let! updatedQuiz =
            quiz
            |> updateQuizWithCurrentQuizzerPrejump
            |> Result.map Score.updateQuizScores

        let events =
            updatedQuiz
            |> createScoreEvents quiz
            |> List.map (fun event ->
                match event with
                | ScoreEvent.TeamScoreChanged teamScoreChanged -> Prejump.Event.TeamScoreChanged teamScoreChanged
                | ScoreEvent.IndividualScoreChanged individualScoreChanged -> Prejump.Event.IndividualScoreChanged individualScoreChanged)
            |> List.append [ (Prejump.Event.Prejump { Quiz = updatedQuiz.Code; Quizzer = (updatedQuiz.CurrentQuizzer |> Option.defaultValue "") }) ]


        return updatedQuiz, events
    }
    |> AsyncResult.ofResult
