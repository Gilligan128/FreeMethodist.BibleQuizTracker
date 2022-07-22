module FreeMethodist.BibleQuizTracker.Server.FailAppeal.Pipeline

open FreeMethodist.BibleQuizTracker.Server.FailAppeal.Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

type UpdateQuiz = Quizzer * TeamPosition -> RunningTeamQuiz -> Result<RunningTeamQuiz, FailAppeal.Error>
type CreateEvents = TeamPosition -> RunningTeamQuiz -> FailAppeal.Event list



let updateQuiz: UpdateQuiz =
    fun (currentQuizzer, teamPosition) quiz ->
        result {
            let changedQuestion =
                quiz.Questions
                |> Map.tryFind quiz.CurrentQuestion
                |> Option.defaultValue QuestionState.initial
                |> fun original -> {original with FailedAppeal = Some currentQuizzer}

            let updatedQuiz =
                changedQuestion
                |> fun changedQuestion ->
                    { quiz with
                        Questions =
                            quiz.Questions
                            |> Map.add quiz.CurrentQuestion changedQuestion }

            return updatedQuiz
        }
