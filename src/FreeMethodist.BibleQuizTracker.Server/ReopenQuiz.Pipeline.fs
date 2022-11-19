module FreeMethodist.BibleQuizTracker.Server.ReopenQuiz.Pipeline

open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow

let private mapCompletedQuizzerToRunning (quizzer: CompletedQuizzer) =
    { Name = quizzer.Name
      Score = quizzer.Score
      Participation = Out }

let private mapCompletedTeamToRunning (team: CompletedTeam) : QuizTeamState =
    { Name = team.Name
      Score = team.Score
      Quizzers =
        team.Quizzers
        |> List.map mapCompletedQuizzerToRunning }

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
        |> List.map (mapCompletedQuizzerToRunning) }

let ofNumber value =
    value
    |> PositiveNumber.create ""
    |> function
        | Ok number -> number
        | Error _ -> PositiveNumber.one

let mapQuestionToRunning (key, value: CompletedQuestion) : PositiveNumber * QuestionState =
    key + 1 |> ofNumber,
    { Prejumps = []
      FailedAppeals = value.FailedAppeals
      AnswerState = QuizAnswer.Complete value.AnswerState }

let updateQuizToRunning (quiz: Choice<CompletedQuiz, OfficialTeamQuiz>) : RunningQuiz =
    match quiz with
    | Choice1Of2 completed ->
        let competitionStyle =
            match completed.CompetitionStyle with
            | CompletedCompetitionStyle.Individual quizzers ->
                quizzers
                |> List.map mapCompletedQuizzerToRunning
                |> RunningCompetitionStyle.Individuals
            | CompletedCompetitionStyle.Team (teamOne, teamTwo) ->
                let runningTeamOne, runningTeamTwo = (mapCompletedTeamToRunning teamOne), (mapCompletedTeamToRunning teamTwo)
                let competitionStyle = (runningTeamOne, runningTeamTwo) |> RunningCompetitionStyle.Team
                competitionStyle

        { Code = completed.Code
          CompetitionStyle = competitionStyle
          CurrentQuestion = (ofNumber completed.CompletedQuestions.Length)
          CurrentQuizzer = None
          Questions =
            completed.CompletedQuestions
            |> List.indexed
            |> List.map mapQuestionToRunning
            |> Map.ofList }
    | Choice2Of2 official ->
        let competitionStyle =
            match official.CompetitionStyle with
            | OfficialCompetitionStyle.Individual quizzers ->
                let competitionStyle = quizzers |> List.map mapCompletedQuizzerToRunning |> RunningCompetitionStyle.Individuals
                competitionStyle
            | OfficialCompetitionStyle.Team (one, two) ->
                let runningTeamOne, runningTeamTwo = (one |> mapOfficialToRunning, two |> mapOfficialToRunning)
                let competitionStyle = (runningTeamOne, runningTeamTwo) |> RunningCompetitionStyle.Team
                competitionStyle

        { Code = official.Code
          CompetitionStyle = competitionStyle
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
