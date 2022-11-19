module FreeMethodist.BibleQuizTracker.Server.Versioning

open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow


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
   
   let private  backwardsCompatibleToFailedAppeals quiz =
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
                            { value with
                                Prejumps =
                                    if value.Prejumps |> isNull then
                                        []
                                    else
                                        value.Prejumps }) }
        | Quiz.Completed quiz ->
            Completed
                { quiz with
                    CompletedQuestions =
                        quiz.CompletedQuestions
                        |> List.map (fun value ->
                            { value with
                                Prejumps =
                                    if value.Prejumps |> isNull then
                                        []
                                    else
                                        value.Prejumps }) }
        | quiz -> quiz
        
   let applyBackwardsCompatibility quiz =
        quiz
        |> backwardsCompatibleToRunningCompetitionStyle
        |> backwardsCompatibleToFailedAppeals
        |> backwardsCompatibleToPrejumps 
 