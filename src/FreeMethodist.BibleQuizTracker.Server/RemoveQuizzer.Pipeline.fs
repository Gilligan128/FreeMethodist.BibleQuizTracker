module FreeMethodist.BibleQuizTracker.Server.RemoveQuizzer_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow

type ValidateRemoval = Quiz -> RemoveQuizzer.Data -> Result<RunningQuiz, RemoveQuizzer.Error>

type RemoveQuizzerFromQuiz = RemoveQuizzer.Data -> RunningQuiz -> Jump seq -> RunningQuiz * CurrentQuizzerChanged option

type CreateEvent = QuizCode -> RemoveQuizzer.Data -> QuizzerNoLongerParticipating
type CreateEvents = QuizCode -> RemoveQuizzer.Data -> CurrentQuizzerChanged option -> RemoveQuizzer.Event list

let validateRemoval (validateQuiz: Quiz -> Result<RunningQuiz, QuizStateError>) : ValidateRemoval =
    fun quiz input ->

        result {
            let! validQuiz =
                validateQuiz quiz
                |> Result.mapError RemoveQuizzer.QuizStateError

            let foundQuizzerOpt =
                validQuiz
                |> RunningQuiz.tryFindQuizzer2 input.Quizzer

            return!
                foundQuizzerOpt
                |> Result.ofOption (RemoveQuizzer.QuizzerNotParticipating input.Quizzer)
                |> Result.map (fun _ -> validQuiz)
        }

let private nextCurrentQuizzer removedQuizzer currentQuizzer =
    if currentQuizzer = removedQuizzer then
        None
    else
        Some currentQuizzer

let removeFromCompetitionStyle removeFromTeam competitionStyle quizzer =
    match competitionStyle with
    | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
        let _, team =
            RunningQuiz.findQuizzerAndTeam (teamOne, teamTwo) quizzer

        let teamOne, teamTwo =
            match team with
            | TeamOne -> removeFromTeam teamOne, teamTwo
            | TeamTwo -> teamOne, removeFromTeam teamTwo

        RunningCompetitionStyle.Team(teamOne, teamTwo)
    | RunningCompetitionStyle.Individuals quizzers ->
        quizzers
        |> List.partition (QuizzerState.isQuizzer quizzer)
        |> snd
        |> RunningCompetitionStyle.Individuals

let removeQuizzerFromQuiz: RemoveQuizzerFromQuiz =
    fun input quiz jumps ->
        let removeFromTeam (team: QuizTeamState) =
            { team with
                Quizzers =
                    team.Quizzers
                    |> List.filter (fun q -> q.Name <> input.Quizzer) }
            |> fun team ->
                { team with
                    Score =
                        quiz.Questions
                        |> Score.createScoreModel
                        |> Score.calculateTeamScore (team.Quizzers |> Seq.map (fun q -> q.Name)) }

        let replaceCurrent =
            quiz.CurrentQuizzer
            |> Option.bind (nextCurrentQuizzer input.Quizzer)

        let newQuiz =
            { quiz with CompetitionStyle = removeFromCompetitionStyle removeFromTeam quiz.CompetitionStyle input.Quizzer }
            |> fun oldQuiz -> { oldQuiz with CurrentQuizzer = replaceCurrent }

        let currentQuizzerChanged =
            if newQuiz.CurrentQuizzer <> quiz.CurrentQuizzer then
                Some
                    { Quiz = quiz.Code
                      CurrentQuizzer = newQuiz.CurrentQuizzer }
            else
                None

        newQuiz, currentQuizzerChanged


let createEvent: CreateEvent =
    fun quizCode input ->
        { Quiz = quizCode
          Quizzer = input.Quizzer }

let createEvents: CreateEvents =
    fun quizCode input currentQuizzerChangedOpt ->
        let currentChangedEvents =
            match currentQuizzerChangedOpt with
            | None -> []
            | Some event -> [ RemoveQuizzer.Event.CurrentQuizzerChanged event ]

        let quizzerRemovedEvent =
            RemoveQuizzer.Event.QuizzerNoLongerParticipating
                { Quiz = quizCode
                  Quizzer = input.Quizzer }

        [ yield quizzerRemovedEvent
          yield! currentChangedEvents ]


let removeQuizzer getQuiz (saveQuiz: SaveQuiz) : RemoveQuizzer.Workflow =
    fun command ->
        asyncResult {
            let! quiz =
                getQuiz command.Quiz
                |> AsyncResult.mapError RemoveQuizzer.DbError

            let! validQuiz =
                validateRemoval validateRunningQuiz quiz command.Data
                |> AsyncResult.ofResult

            let quiz, currentChangedEvent =
                removeQuizzerFromQuiz command.Data validQuiz []

            do!
                quiz
                |> Running
                |> saveQuiz
                |> AsyncResult.mapError RemoveQuizzer.DbError

            return createEvents command.Quiz command.Data currentChangedEvent
        }
