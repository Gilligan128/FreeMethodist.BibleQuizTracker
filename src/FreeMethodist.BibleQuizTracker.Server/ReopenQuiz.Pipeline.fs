module FreeMethodist.BibleQuizTracker.Server.ReopenQuiz.Pipeline

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow

let private mapCompletedTeamToRunning (team: CompletedTeam) : QuizTeamState =
    { Name = team.Name
      Score = team.Score
      Quizzers =
        team.Quizzers
        |> List.map (fun q ->
            { Name = q.Name
              Score = q.Score
              Participation = Out }) }

let ofNumber value =
    value
    |> PositiveNumber.create ""
    |> function
        | Ok number -> number
        | Error _ -> PositiveNumber.one

let updateQuizToRunning (quiz: Choice<CompletedQuiz, OfficialTeamQuiz>) : RunningTeamQuiz =
    match quiz with
    | Choice1Of2 completed ->
        let teamOne, teamTwo =
            match completed.CompetitionStyle with
            | CompletedCompetitionStyle.Individual _ ->
                { Name = ""
                  Score = TeamScore.initial
                  Quizzers = [] },
                { Name = ""
                  Score = TeamScore.initial
                  Quizzers = [] }
            | CompletedCompetitionStyle.Team (teamOne, teamTwo) ->
                (mapCompletedTeamToRunning teamOne), (mapCompletedTeamToRunning teamTwo)

        { Code = completed.Code
          TeamOne = teamOne
          TeamTwo = teamTwo
          CurrentQuestion = (ofNumber completed.CompletedQuestions.Length)
          CurrentQuizzer = None
          Questions =
            completed.CompletedQuestions
            |> List.indexed
            |> List.map (fun (key, value) ->
                key + 1 |> ofNumber,
                { FailedAppeal = value.FailedAppeal
                  AnswerState = QuizAnswer.Complete value.AnswerState })
            |> Map.ofList }
    | Choice2Of2 official -> Unchecked.defaultof<RunningTeamQuiz>

let reopenQuiz getQuiz saveQuiz =
    fun command ->
        asyncResult {
            let! quiz =
                command
                |> getQuiz
                |> AsyncResult.mapError ReopenQuiz.DbError

            let! validQuiz =
                validateCompleteQuiz quiz
                |> AsyncResult.ofResult
                |> AsyncResult.mapError ReopenQuiz.QuizState

            let runningQuiz =
                validQuiz |> updateQuizToRunning

            do!
                runningQuiz
                |> Running
                |> saveQuiz
                |> AsyncResult.mapError ReopenQuiz.DbError

            return
                [ ReopenQuiz.Event.QuizStateChanged
                      { Quiz = runningQuiz.Code
                        NewState = nameof Running } ]
        }
