module FreeMethodist.BibleQuizTracker.Server.AddQuizzer_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open Microsoft.FSharp.Core

type ValidateQuizzerAdd = Quiz -> AddQuizzer.Data -> Result<RunningQuiz, AddQuizzer.Error>
type AddQuizzerToQuiz = RunningQuiz -> AddQuizzer.Data -> RunningQuiz
type CreateEvent = QuizCode -> AddQuizzer.Data -> QuizzerParticipating

let validateQuizzer participatingQuizzers quizzer =
    if participatingQuizzers |> Seq.contains quizzer then
        Error(AddQuizzer.Error.QuizzerAlreadyAdded quizzer)
    else
        Ok()

let validateQuizzerAdd (validateQuiz: ValidateQuizIsRunning) : ValidateQuizzerAdd =
    fun teamQuiz input ->
        result {
            let! quiz =
                validateQuiz teamQuiz
                |> Result.mapError AddQuizzer.Error.QuizState

            let getQuizzerNames (quizzers: List<QuizzerState>) =
                quizzers
                |> List.map (fun q -> q.Name)
                |> List.toSeq

            let participatingQuizzers =
                match quiz.CompetitionStyle with
                | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
                    teamOne.Quizzers @ teamTwo.Quizzers
                    |> getQuizzerNames
                | RunningCompetitionStyle.Individuals quizzerStates -> quizzerStates |> getQuizzerNames

            do! validateQuizzer participatingQuizzers input.Name
            return quiz
        }

let newQuizzer scoring name scoreModel =
    { Name = name
      Participation = ParticipationState.In
      Score =
        scoreModel
        |> Score.eventsForQuizzers [ name ]
        |> Score.calculate scoring }

let newTeamState name scoreModel (originalTeamState: QuizTeamState) =

    { originalTeamState with
        Quizzers =
            originalTeamState.Quizzers
            @ [ newQuizzer Score.quizzerTeamStyleScoring name scoreModel ] }
    |> fun teamstate ->
        { teamstate with
            Score =
                scoreModel
                |> Score.eventsForQuizzers [ name ]
                |> Score.calculate Score.teamScoring }

let newIndividualRoster scoreModel name quizzers =
    quizzers
    @ [ newQuizzer Score.quizzerIndividualStyleScoring name scoreModel ]


let addQuizzerToQuiz: AddQuizzerToQuiz =
    fun quiz input ->
        let scoreModel =
            quiz.Questions |> Score.createScoreModel

        let newTeamState =
            newTeamState input.Name scoreModel

        match input.Team, quiz.CompetitionStyle with
        | None, RunningCompetitionStyle.Team _ -> quiz
        | Some TeamOne, RunningCompetitionStyle.Team (teamOne, teamTwo) ->
            { quiz with
                TeamOne = newTeamState teamOne
                CompetitionStyle = RunningCompetitionStyle.Team(newTeamState teamOne, teamTwo) }
        | Some TeamTwo, RunningCompetitionStyle.Team (teamOne, teamTwo) ->
            { quiz with
                TeamTwo = newTeamState teamTwo
                CompetitionStyle = RunningCompetitionStyle.Team(teamOne, newTeamState teamTwo) }
        | _, RunningCompetitionStyle.Individuals quizzerStates ->
            { quiz with
                CompetitionStyle =
                    RunningCompetitionStyle.Individuals(
                        quizzerStates
                        |> newIndividualRoster scoreModel input.Name
                    ) }

let createEvent: CreateEvent =
    fun code input -> { Quizzer = input.Name; Quiz = code }

let addQuizzerAsync getQuiz (saveQuiz: SaveQuiz) : AddQuizzer.Workflow =
    fun command ->
        asyncResult {
            let! quiz =
                getQuiz command.Quiz
                |> AsyncResult.mapError AddQuizzer.DbError

            let! validQuiz =
                validateQuizzerAdd (validateRunningQuiz) quiz command.Data
                |> Async.retn

            return!
                addQuizzerToQuiz validQuiz command.Data
                |> Running
                |> saveQuiz
                |> AsyncResult.mapError AddQuizzer.Error.DbError

            return createEvent command.Quiz command.Data
        }
