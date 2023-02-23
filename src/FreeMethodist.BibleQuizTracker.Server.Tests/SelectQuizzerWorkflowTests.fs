module FreeMethodist.BibleQuizTracker.Server.Tests.SelectQuizzerWorkflowTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

[<Fact>]
let ``When Selecting Quizzer then that is now the quiz's current quizzer`` () =
    let quiz = RunningQuiz.newTeamQuiz

    let result =
        SelectQuizzer_Pipeline.changeCurrentQuizzer "Jordan" quiz

    Assert.Equal(Some "Jordan", result.CurrentQuizzer)

[<Fact>]
let ``Given an Individual quiz, When valiating a quizzer participating, then validation is successful`` () =
    let quizzer = "Jordan"
    let quiz = RunningQuiz.newTeamQuiz
                |> fun quiz -> {quiz with CompetitionStyle = RunningCompetitionStyle.Individuals [QuizzerState.create quizzer]}
                
    let result = SelectQuizzer_Pipeline.validateSelection ( Quiz.Running quiz) { Quizzer = quizzer }
    
    Assert.Equal(Ok quiz, result)