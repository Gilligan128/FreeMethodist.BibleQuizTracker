module FreeMethodist.BibleQuizTracker.Server.QuizState_Versioning

open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open Azure.Core.Serialization
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Tournament
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.Azure.Cosmos



[<RequireQualifiedAccess>]
module QuizVersioning =

    
    let isNull value = obj.ReferenceEquals(value, null)

    let private backwardsCompatibleToRunningCompetitionStyle quiz =
        match quiz with
        | Quiz.Running quiz when quiz.CompetitionStyle |> isNull ->
            let initialTeam =
                { Score = QuizScore.zero
                  Quizzers = []
                  Name = "" }

            Running { quiz with CompetitionStyle = RunningCompetitionStyle.Team(initialTeam, initialTeam) }
        | quiz -> quiz

    let private backwardsCompatibleToFailedAppeals quiz =
        match quiz with
        | Quiz.Running quiz ->
            Running
                { quiz with
                    Questions =
                        quiz.Questions
                        |> Map.map (fun _ value ->
                            { value with
                                FailedAppeals =
                                    if value.FailedAppeals |> isNull then
                                        []
                                    else
                                        value.FailedAppeals }) }
        | Quiz.Completed quiz ->
            Completed
                { quiz with
                    CompletedQuestions =
                        quiz.CompletedQuestions
                        |> List.map (fun value ->
                            { value with
                                FailedAppeals =
                                    if value.FailedAppeals |> isNull then
                                        []
                                    else
                                        value.FailedAppeals }) }
        | quiz -> quiz

    let private backwardsCompatibleToPrejumps quiz =
        match quiz with
        | Quiz.Running quiz ->
            Running
                { quiz with
                    Questions =
                        quiz.Questions
                        |> Map.map (fun _ value ->
                            { value with Prejumps = if value.Prejumps |> isNull then [] else value.Prejumps }) }
        | Quiz.Completed quiz ->
            Completed
                { quiz with
                    CompletedQuestions =
                        quiz.CompletedQuestions
                        |> List.map (fun value ->
                            { value with Prejumps = if value.Prejumps |> isNull then [] else value.Prejumps }) }
        | quiz -> quiz

    let private backwardsCompatibleTournamentLink quiz =
        match quiz with
        | Quiz.Running runningQuiz when runningQuiz.TournamentInfo |> isNull ->
            Running { runningQuiz with TournamentInfo = TournamentInfo.empty }
        | Quiz.Completed completedQuiz when completedQuiz.TournamentInfo |> isNull ->
            Completed { completedQuiz with TournamentInfo = TournamentInfo.empty }
        | Quiz.Official officialTeamQuiz when officialTeamQuiz.TournamentInfo |> isNull ->
            Official { officialTeamQuiz with TournamentInfo = TournamentInfo.empty }
        | quiz -> quiz 


    let applyBackwardsCompatibility quiz =
        quiz
        |> backwardsCompatibleToRunningCompetitionStyle
        |> backwardsCompatibleToFailedAppeals
        |> backwardsCompatibleToPrejumps
        |> backwardsCompatibleTournamentLink

type QuizBackwardsCompatibilityConverter () =                                                                    
     inherit JsonConverter<Quiz>()
  
        override this.CanConvert(typeToConvert) =
            typeToConvert = typeof<Quiz>

        override this.Read(reader, typeToConvert, options) =
            
            let quiz = JsonSerializer.Deserialize<Quiz>(&reader, options)
            quiz |> QuizVersioning.applyBackwardsCompatibility
        override this.Write(writer, value, options) =
            JsonSerializer.Serialize<Quiz>(writer, value, options)
        