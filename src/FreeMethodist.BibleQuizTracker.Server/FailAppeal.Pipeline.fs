module FreeMethodist.BibleQuizTracker.Server.FailAppeal.Pipeline

open FreeMethodist.BibleQuizTracker.Server.FailAppeal.Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

type UpdateQuiz = Quizzer * TeamPosition -> RunningTeamQuiz -> Result<RunningTeamQuiz, FailAppeal.Error>
type CreateEvents = TeamPosition -> RunningTeamQuiz -> FailAppeal.Event list



let updateQuiz: UpdateQuiz =
    fun (currentQuizzer, teamPosition) quiz ->
        result {
            let changedQuestion, revertingAppealer =
                quiz.Questions
                |> Map.tryFind quiz.CurrentQuestion
                |> Option.defaultValue QuestionState.initial
                |> fun original -> { original with FailedAppeal = Some currentQuizzer }, original.FailedAppeal
               
        
            let updateCurrentQuestion changedQuestion quiz =
                changedQuestion
                |> fun changedQuestion ->
                    { quiz with
                        Questions =
                            quiz.Questions
                            |> Map.add quiz.CurrentQuestion changedQuestion }

            let updateFailingTeamScore teamPosition (quiz: RunningTeamQuiz) =
                let updateScore (team: QuizTeamState) =
                    { team with Score = team.Score |> TeamScore.failAppeal }

                match teamPosition with
                | TeamOne -> { quiz with TeamOne = updateScore quiz.TeamOne }
                | TeamTwo -> { quiz with TeamTwo = updateScore quiz.TeamTwo }
            
            let updateRevertedAppealTeamScore teamPositionOpt (quiz: RunningTeamQuiz) =
                let updateScore (team: QuizTeamState) =
                    { team with Score = team.Score |> TeamScore.revertAppealFailure }

                match teamPositionOpt with
                | Some TeamOne -> { quiz with TeamOne = updateScore quiz.TeamOne }
                | Some TeamTwo -> { quiz with TeamTwo = updateScore quiz.TeamTwo }
                | None -> quiz
                
            let revertedQuizzerOpt =
               revertingAppealer |> Option.bind (fun q -> RunningTeamQuiz.tryFindQuizzerAndTeam q quiz) |> Option.map snd
                
            let updatedQuiz =
                quiz
                |> updateCurrentQuestion changedQuestion
                |> updateFailingTeamScore teamPosition
                |> updateRevertedAppealTeamScore revertedQuizzerOpt

            return updatedQuiz
        }
