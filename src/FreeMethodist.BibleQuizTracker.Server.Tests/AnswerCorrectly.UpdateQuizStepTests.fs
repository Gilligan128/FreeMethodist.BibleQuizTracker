module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerCorrectly.UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Tests.Common
open FreeMethodist.BibleQuizTracker.Server.Tests.Quiz
open FreeMethodist.BibleQuizTracker.Server.Workflows
open Xunit


let quizWithQuizzerOnTeamOne quizzer =
    { RunningTeamQuiz.identity with TeamOne = { RunningTeamQuiz.identity.TeamOne with Quizzers = [ quizzer ] } }

[<Fact>]
let ``Quizzer Answers who is not participating results in error`` () =
    let quiz =
        { RunningTeamQuiz.identity with
            TeamOne = { RunningTeamQuiz.identity.TeamOne with Quizzers = [ QuizzerState.create "Jim" ] }
            TeamTwo = { RunningTeamQuiz.identity.TeamTwo with Quizzers = [ QuizzerState.create "Jessie" ] } }

    let quizzer = "Not Participating"

    let result = updateQuiz quizzer quiz

    let error =
        Error(AnswerCorrectly.Error.QuizzerNotFound quizzer)

    Assert.Equal(error, result)

[<Fact>]
let ``When Quizzer Answers then individual score goes up`` () =
    let quizzer = QuizzerState.create "Jim"

    let initialQuiz =
        quizWithQuizzerOnTeamOne quizzer

    let result =
        updateQuiz quizzer.Name initialQuiz

    let expectedScore =
        TeamScore.initial |> TeamScore.correctAnswer

    assertSuccess result (fun (updatedQuiz) ->
        let updatedQuizzer =
            updatedQuiz.QuizState.TeamOne.Quizzers
            |> List.find (fun q -> q.Name = quizzer.Name)

        Assert.Equal(expectedScore, updatedQuizzer.Score))

[<Fact>]
let ``When Quizzer Answers then team score goes up`` () =
    let quizzer = QuizzerState.create "Jim"

    let initialQuiz =
        quizWithQuizzerOnTeamOne quizzer

    let result =
        updateQuiz quizzer.Name initialQuiz

    let expectedScore = TeamScore.ofQuestions 1
    assertSuccess result (fun (updatedQuiz) -> Assert.Equal(expectedScore, updatedQuiz.QuizState.TeamOne.Score))


[<Fact>]
let ``When Quizzer Answers then only updates score of answering quizzer`` () =
    let answerer =
        QuizzerState.create "Answerer"

    let nonAnswerer = QuizzerState.create "Jim"

    let quiz =
        { RunningTeamQuiz.identity with
            TeamOne = { RunningTeamQuiz.identity.TeamOne with Quizzers = [ nonAnswerer; answerer ] }
            TeamTwo = { RunningTeamQuiz.identity.TeamTwo with Quizzers = [ QuizzerState.create "Jessie" ] } }

    let result = updateQuiz answerer.Name quiz

    assertSuccess result (fun (updatedQuiz) ->
        let updatedQuizzer =
            updatedQuiz.QuizState.TeamOne.Quizzers
            |> List.find (fun q -> q.Name = nonAnswerer.Name)

        Assert.Equal(nonAnswerer.Score, updatedQuizzer.Score))

[<Fact>]
let ``When Quizzer Answers then increment the current question`` () =
    let quizzer = QuizzerState.create "Jim"

    let initialQuiz =
        { quizWithQuizzerOnTeamOne quizzer with CurrentQuestion = PositiveNumber.one }

    let result =
        updateQuiz quizzer.Name initialQuiz

    assertSuccess result (fun (updatedQuiz) ->
        Assert.Equal(
            initialQuiz.CurrentQuestion
            |> PositiveNumber.increment,
            updatedQuiz.QuizState.CurrentQuestion
        ))

[<Fact>]
let ``When Quizzer Answers then record answered question for history`` () =

    result {
        let quizzer = QuizzerState.create "Jim"

        let initialQuiz =
            { (quizWithQuizzerOnTeamOne quizzer) with CurrentQuestion = PositiveNumber.one }

        let! result = updateQuiz quizzer.Name initialQuiz

        let expectedAnswer =
            { Answerer = quizzer.Name
              IncorrectAnswerers = [] }
            |> CompletedQuestion.Answered
            |> QuizAnswer.Complete

        Assert.Equal(
            Some expectedAnswer,
            result.QuizState.Questions.TryFind initialQuiz.CurrentQuestion
            |> Option.map (fun q -> q.AnswerState)
        )
    }

[<Fact>]
let ``Given Quizzer already answered correctly When Quizzer Answers then Error`` () =
    let quizzer = QuizzerState.create "Jim"

    let alreadyAnswered =
        { Answerer = quizzer.Name
          IncorrectAnswerers = [] }
        |> Answered
        |> Complete

    let quizWithQuizzerOnTeamOne =
        (quizWithQuizzerOnTeamOne quizzer)

    let initialQuiz =
        { quizWithQuizzerOnTeamOne with
            CurrentQuestion = PositiveNumber.one}
        |> insertCurrentAnswer alreadyAnswered

    let result =
        updateQuiz quizzer.Name initialQuiz 

    Assert.Equal(
        Result.Error(
            AnswerCorrectly.QuizzerAlreadyAnsweredCorrectly(
                QuizAnswer.QuizzerAlreadyAnsweredCorrectly(quizzer.Name, initialQuiz.CurrentQuestion)
            )
        ),
        result
    )

[<Fact>]
let ``Given someone else previously answered correctly  When Quizzer Answers then revert previous quizzer's score`` () =
    let quizzer = QuizzerState.create "Jim"

    let previousAnswerer =
        QuizzerState.create "Previous"

    let alreadyAnsweredQuestion =
        { Answerer = previousAnswerer.Name
          IncorrectAnswerers = [] }
        |> Answered
        |> Complete

    let setupQuiz (quiz: RunningTeamQuiz) =
        { quiz with
            TeamOne = { quiz.TeamOne with Quizzers = [ quizzer; previousAnswerer ] }
         }

    let initialQuiz =
        RunningTeamQuiz.identity |> setupQuiz |> insertCurrentAnswer alreadyAnsweredQuestion

    result {
        let! result = updateQuiz quizzer.Name initialQuiz

        let revertedQuizzerState =
            result.QuizState.TeamOne.Quizzers
            |> List.find (QuizzerState.isQuizzer previousAnswerer.Name)

        let expectedScore =
            previousAnswerer.Score
            |> TeamScore.revertCorrectAnswer

        Assert.Equal(expectedScore, revertedQuizzerState.Score)
    }

[<Fact>]
let ``Given someone else previously answered correctly from other team When Quizzer Answers then revert previous quizzer's score``
    ()
    =
    let quizzer = QuizzerState.create "Jim"

    let previousAnswerer =
        QuizzerState.create "Previous"

    let alreadyAnswered =
        { Answerer = previousAnswerer.Name
          IncorrectAnswerers = [] }
        |> Answered
        |> Complete

    let setupQuiz (quiz: RunningTeamQuiz) =
        { quiz with
            TeamOne = { quiz.TeamOne with Quizzers = [ quizzer ] }
            TeamTwo = { quiz.TeamTwo with Quizzers = [ previousAnswerer ] }

         }

    let initialQuiz =
        RunningTeamQuiz.identity |> setupQuiz |> insertCurrentAnswer alreadyAnswered

    result {
        let! result = updateQuiz quizzer.Name initialQuiz

        let revertedScore =
            result.QuizState.TeamTwo.Score

        let expectedScore =
            initialQuiz.TeamTwo.Score
            |> TeamScore.revertCorrectAnswer

        Assert.Equal(expectedScore, revertedScore)
    }

[<Fact>]
let ``Given someone else previously answered correctly from same team When Quizzer Answers then team score is unchanged``
    ()
    =
    let quizzer = QuizzerState.create "Jim"

    let previousAnswerer =
        QuizzerState.create "Previous"

    let alreadyAnsweredQuestion =
        { Answerer = previousAnswerer.Name
          IncorrectAnswerers = [] }
        |> Answered
        |> Complete

    let setupQuiz (quiz: RunningTeamQuiz) =
        { quiz with
            TeamOne = { quiz.TeamOne with Quizzers = [ quizzer; previousAnswerer ] }
         }

    let initialQuiz =
        RunningTeamQuiz.identity |> setupQuiz |> insertCurrentAnswer alreadyAnsweredQuestion

    result {
        let! result = updateQuiz quizzer.Name initialQuiz

        let revertedScore =
            result.QuizState.TeamOne.Score

        let expectedScore =
            initialQuiz.TeamOne.Score

        Assert.Equal(expectedScore, revertedScore)
    }
