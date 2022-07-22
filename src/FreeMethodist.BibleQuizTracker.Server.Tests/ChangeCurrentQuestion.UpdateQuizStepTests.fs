module FreeMethodist.BibleQuizTracker.Server.Tests.ChangeCurrentQuestion_UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit

[<Fact>]
let ``Given this is the first time a question is current When Changing Question then record an Unanswered Question`` () =
    let initialQuiz = RunningTeamQuiz.identity
    let nextQuestion = initialQuiz.CurrentQuestion |> PositiveNumber.increment
    let result = ChangeCurrentQuestion_Pipeline.updateQuiz initialQuiz nextQuestion
    let expectedQuestion = []  |> Unanswered |> Complete
    
    Assert.Equal(expectedQuestion, result.QuestionsDeprecated[nextQuestion])
    Assert.Equal(expectedQuestion, result.Questions[nextQuestion].AnswerState)
    
    