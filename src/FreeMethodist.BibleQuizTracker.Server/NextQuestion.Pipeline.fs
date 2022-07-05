module FreeMethodist.BibleQuizTracker.Server.NextQuestion_Pipeline

open FreeMethodist.BibleQuizTracker.Server.NextQuestion_Workflow
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain

type MoveQuizToQuestion = Quiz -> Result<RunningTeamQuiz, QuizStateError>
type CreateEvent = RunningTeamQuiz  -> QuestionChanged


let moveQuizToNextQuestion quiz =
    Ok quiz
let createEvent (quiz: RunningTeamQuiz)  =
    { Quiz = quiz.Code; NewQuestion = quiz.CurrentQuestion }

let moveQuizToQuestion getQuiz saveQuiz : NextQuestion.Workflow =
    fun command ->
        result {
            let quiz = getQuiz command.Quiz
            let! newQuizState = moveQuizToNextQuestion quiz
            saveQuiz quiz
            return createEvent newQuizState
        }
