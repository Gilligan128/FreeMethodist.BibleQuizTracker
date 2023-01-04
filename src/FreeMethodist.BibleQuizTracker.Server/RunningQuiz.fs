namespace global
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Tournament

type RunningQuiz =
    { Code: QuizCode
      Questions: Map<PositiveNumber, QuestionState>
      CompetitionStyle: RunningCompetitionStyle
      CurrentQuestion: QuestionNumber
      CurrentQuizzer: Quizzer option
      TournamentInfo: TournamentLink }

[<RequireQualifiedAccess>]
module RunningQuiz =
    let newTeamQuiz =
        { Code = "Example"
          CurrentQuestion = PositiveNumber.one
          CurrentQuizzer = None
          Questions = Map.empty
          CompetitionStyle =
            RunningCompetitionStyle.Team(
                { Name = "LEFT"
                  Score = QuizScore.zero
                  Quizzers = [] },
                { Name = "RIGHT"
                  Score = QuizScore.zero
                  Quizzers = [] }
            )
          TournamentInfo = TournamentInfo.empty |> Info }

    let newIndividualQuiz =
        { Code = "Example"
          CurrentQuestion = PositiveNumber.one
          CurrentQuizzer = None
          Questions = Map.empty
          CompetitionStyle = RunningCompetitionStyle.Individuals []
          TournamentInfo = TournamentInfo.empty |> Info }

    let getTeam teamPosition (quiz: RunningQuiz) =
        match quiz.CompetitionStyle, teamPosition with
        | RunningCompetitionStyle.Individuals _, _ -> { Name = ""; Score = QuizScore.zero; Quizzers = [] }
        | RunningCompetitionStyle.Team (teamOne, _), TeamOne -> teamOne
        | RunningCompetitionStyle.Team (_, teamTwo), TeamTwo -> teamTwo

    let getTeamScore teamPosition (quiz: RunningQuiz) = (getTeam teamPosition quiz).Score

    let findQuizzerAndTeam (teamOne, teamTwo) quizzer =
        [ yield!
              (teamOne.Quizzers
               |> List.map (fun q -> (q, TeamOne)))
          yield!
              (teamTwo.Quizzers
               |> List.map (fun q -> (q, TeamTwo))) ]
        |> List.find (fun (q, _) -> QuizzerState.isQuizzer quizzer q)

    let tryFindQuizzerAndTeam (teamOne, teamTwo) quizzer =
        [ yield!
              (teamOne.Quizzers
               |> List.map (fun q -> (q, TeamOne)))
          yield!
              (teamTwo.Quizzers
               |> List.map (fun q -> (q, TeamTwo))) ]
        |> List.tryFind (fun (q, _) -> QuizzerState.isQuizzer quizzer q)

    let findQuizzer quizzer (quiz: RunningQuiz) =
        match quiz.CompetitionStyle with
        | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
            let quizzer, team = findQuizzerAndTeam (teamOne, teamTwo) quizzer

            quizzer, Some team
        | RunningCompetitionStyle.Individuals quizzerStates ->
            quizzerStates
            |> List.find (QuizzerState.isQuizzer quizzer),
            None

    let tryFindQuizzer2 quizzer (quiz: RunningQuiz) =
        match quiz.CompetitionStyle with
        | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
            tryFindQuizzerAndTeam (teamOne, teamTwo) quizzer
            |> Option.map (fun (q, team) -> (q, Some team))
        | RunningCompetitionStyle.Individuals quizzerStates ->
            quizzerStates
            |> List.tryFind (QuizzerState.isQuizzer quizzer)
            |> Option.map (fun q -> q, None)

    let tryFindQuizzer (quiz: RunningQuiz) quizzer =
        match quiz.CompetitionStyle with
        | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
            tryFindQuizzerAndTeam (teamOne, teamTwo) quizzer
            |> Option.map (fun (q, team) -> (q, Some team))
        | RunningCompetitionStyle.Individuals quizzerStates ->
            quizzerStates
            |> List.tryFind (QuizzerState.isQuizzer quizzer)
            |> Option.map (fun q -> q, None)

    let changeCurrentAnswer quiz changedQuestion =
        quiz.Questions
        |> Map.change quiz.CurrentQuestion (fun q ->
            q
            |> Option.defaultValue QuestionState.initial
            |> fun q -> { q with AnswerState = changedQuestion }
            |> Some)

    let private updateQuizzerScore changeScore (quizzer: QuizzerState) =
        { quizzer with Score = quizzer.Score |> changeScore }

    let private updateAnsweringQuizzer changeScore quizzerName quizzers =
        quizzers
        |> List.map (fun q ->
            if QuizzerState.isQuizzer quizzerName q then
                (updateQuizzerScore changeScore q)
            else
                q)


    let private updateTeam changeScore quizzerName (team: QuizTeamState) =
        { team with
            Quizzers =
                team.Quizzers
                |> updateAnsweringQuizzer changeScore quizzerName
            Score = team.Score |> changeScore }

    let private updateTeamAndQuizzerScore changeScore (quiz: RunningQuiz ) (teamOne, teamTwo) quizzerName =
        let updateTeam = updateTeam changeScore quizzerName

        quizzerName
        |> tryFindQuizzerAndTeam (teamOne, teamTwo)
        |> Option.map (function
            | _, TeamOne ->
                { quiz with
                    CompetitionStyle = RunningCompetitionStyle.Team(updateTeam teamOne, teamTwo) }
            | _, TeamTwo ->
                { quiz with
                    CompetitionStyle = RunningCompetitionStyle.Team(teamOne, updateTeam teamTwo) })

    let updateIndividualQuizzerScore changeScore quizzerName (updatedQuizInfo: RunningQuiz) quizzerStates =
        let quizzerExistsResult =
            if
                quizzerStates
                |> List.exists (QuizzerState.isQuizzer quizzerName)
            then
                Some quizzerStates
            else
                None

        quizzerExistsResult
        |> Option.map (fun quizzerStates ->
            { updatedQuizInfo with
                CompetitionStyle =
                    quizzerStates
                    |> updateAnsweringQuizzer changeScore quizzerName
                    |> RunningCompetitionStyle.Individuals })

    let updateScoresBasedOnQuizzer changeScore quizzerName (updatedQuizInfo: RunningQuiz) =
        match updatedQuizInfo.CompetitionStyle with
        | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
            updateTeamAndQuizzerScore
                changeScore
                updatedQuizInfo
                (teamOne, teamTwo)
                quizzerName
        | RunningCompetitionStyle.Individuals quizzerStates ->
            updateIndividualQuizzerScore changeScore quizzerName updatedQuizInfo quizzerStates

    let teamOneScore (quiz: RunningQuiz) =
        match quiz.CompetitionStyle with
        | RunningCompetitionStyle.Team (teamOne, _) -> Some teamOne.Score
        | RunningCompetitionStyle.Individuals _ -> None

    let updateTeamScore updateScore teamPosition (quiz: RunningQuiz) =
        let updateScore (team: QuizTeamState) =
            { team with Score = team.Score |> updateScore }

        match quiz.CompetitionStyle with
        | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
            match teamPosition with
            | TeamOne ->
                { quiz with
                    CompetitionStyle = RunningCompetitionStyle.Team(updateScore teamOne, teamTwo) }
            | TeamTwo ->
                { quiz with
                    CompetitionStyle = RunningCompetitionStyle.Team(teamOne, updateScore teamTwo) }
        | RunningCompetitionStyle.Individuals _ -> quiz
