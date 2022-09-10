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

let private mapOptionToList opt =
    match opt with
    | Some value -> [ value ]
    | None -> []

let private mapOfficialToRunning (team: OfficialTeam) : QuizTeamState =
    { Name = team.Name
      Score = team.Score
      Quizzers =
        team
        |> OfficialTeam.quizzerList
        |> List.choose id
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

let mapQuestionToRunning (key, value: CompletedQuestion) : PositiveNumber * QuestionState =
    key + 1 |> ofNumber,
    { FailedAppeal = value.FailedAppeal
      AnswerState = QuizAnswer.Complete value.AnswerState }

let updateQuizToRunning (quiz: Choice<CompletedQuiz, OfficialTeamQuiz>) : RunningTeamQuiz =
    let emptyTeam =
        { Name = ""
          Score = TeamScore.zero
          Quizzers = [] }

    match quiz with
    | Choice1Of2 completed ->
        let teamOne, teamTwo =
            match completed.CompetitionStyle with
            | CompletedCompetitionStyle.Individual _ -> emptyTeam, emptyTeam
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
            |> List.map mapQuestionToRunning
            |> Map.ofList }
    | Choice2Of2 official ->
        let teamOne, teamTwo =
            match official.CompetitionStyle with
            | OfficialCompetitionStyle.Individual _ -> emptyTeam, emptyTeam
            | OfficialCompetitionStyle.Team (one, two) -> (one |> mapOfficialToRunning, two |> mapOfficialToRunning)

        { Code = official.Code
          TeamOne = teamOne
          TeamTwo = teamTwo
          CurrentQuestion = official.CompletedQuestions.Length |> ofNumber
          CurrentQuizzer = None
          Questions =
            official.CompletedQuestions
            |> List.indexed
            |> List.map mapQuestionToRunning
            |> Map.ofList }

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
