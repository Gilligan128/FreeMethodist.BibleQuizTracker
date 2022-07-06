module FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Pipeline

open FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain

type MoveQuizToQuestion = Quiz -> Result<RunningTeamQuiz, QuizStateError>
type CreateEvent = RunningTeamQuiz  -> QuestionChanged


let updateQuiz quiz question =
   { quiz with CurrentQuestion = question }
let createEvent (quiz: RunningTeamQuiz)  =
    { Quiz = quiz.Code; NewQuestion = quiz.CurrentQuestion }

let moveQuizToQuestion (getQuiz:GetTeamQuiz) (saveQuiz:SaveTeamQuiz) : MoveQuestion.Workflow =
    fun command ->
        result {
            let quiz = getQuiz command.Quiz
            let! runningQuiz = validateQuiz quiz
            let newQuizState = updateQuiz runningQuiz command.Data.Question
            saveQuiz quiz
            return createEvent newQuizState
        }
