module FreeMethodist.BibleQuizTracker.Server.Tests.AnswerCorrectly.UpdateQuizStepTests

open FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Pipeline
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.Tests.Common
open FreeMethodist.BibleQuizTracker.Server.Tests.Quiz
open Xunit


let quizWithQuizzerOnTeamOne quizzer =
    { RunningQuiz.identity with TeamOne = { RunningQuiz.identity.TeamOne with Quizzers = [ quizzer ] } }

[<Fact>]
let ``Quizzer Answers who is not participating results in error`` () =
    let quiz =
        { RunningQuiz.identity with
            TeamOne = { RunningQuiz.identity.TeamOne with Quizzers = [ QuizzerState.create "Jim" ] }
            TeamTwo = { RunningQuiz.identity.TeamTwo with Quizzers = [ QuizzerState.create "Jessie" ] } }

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
        QuizScore.zero |> QuizScore.correctAnswer

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

    let expectedScore = QuizScore.ofQuestions 1
    assertSuccess result (fun (updatedQuiz) -> Assert.Equal(expectedScore, updatedQuiz.QuizState.TeamOne.Score))


[<Fact>]
let ``When Quizzer Answers then only updates score of answering quizzer`` () =
    let answerer =
        QuizzerState.create "Answerer"

    let nonAnswerer = QuizzerState.create "Jim"

    let quiz =
        { RunningQuiz.identity with
            TeamOne = { RunningQuiz.identity.TeamOne with Quizzers = [ nonAnswerer; answerer ] }
            TeamTwo = { RunningQuiz.identity.TeamTwo with Quizzers = [ QuizzerState.create "Jessie" ] } }

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
            |> CompletedAnswer.Answered
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
        { quizWithQuizzerOnTeamOne with CurrentQuestion = PositiveNumber.one }
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

    let setupQuiz (quiz: RunningQuiz) =
        { quiz with TeamOne = { quiz.TeamOne with Quizzers = [ quizzer; previousAnswerer ] } }

    let initialQuiz =
        RunningQuiz.identity
        |> setupQuiz
        |> insertCurrentAnswer alreadyAnsweredQuestion

    result {
        let! result = updateQuiz quizzer.Name initialQuiz

        let revertedQuizzerState =
            result.QuizState.TeamOne.Quizzers
            |> List.find (QuizzerState.isQuizzer previousAnswerer.Name)

        let expectedScore =
            previousAnswerer.Score
            |> QuizScore.revertCorrectAnswer

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

    let setupQuiz (quiz: RunningQuiz) =
        { quiz with
            TeamOne = { quiz.TeamOne with Quizzers = [ quizzer ] }
            TeamTwo = { quiz.TeamTwo with Quizzers = [ previousAnswerer ] }

         }

    let initialQuiz =
        RunningQuiz.identity
        |> setupQuiz
        |> insertCurrentAnswer alreadyAnswered

    result {
        let! result = updateQuiz quizzer.Name initialQuiz

        let revertedScore =
            result.QuizState.TeamTwo.Score

        let expectedScore =
            initialQuiz.TeamTwo.Score
            |> QuizScore.revertCorrectAnswer

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

    let setupQuiz (quiz: RunningQuiz) =
        { quiz with TeamOne = { quiz.TeamOne with Quizzers = [ quizzer; previousAnswerer ] } }

    let initialQuiz =
        RunningQuiz.identity
        |> setupQuiz
        |> insertCurrentAnswer alreadyAnsweredQuestion

    result {
        let! result = updateQuiz quizzer.Name initialQuiz

        let revertedScore =
            result.QuizState.TeamOne.Score

        let expectedScore =
            initialQuiz.TeamOne.Score

        Assert.Equal(expectedScore, revertedScore)
    }

[<Fact>]
let ``Given Individual quiz When quizzer answers correctly then update their score`` () =
    let answerer = QuizzerState.create "Jim"

    let previouslyUnanswered =
        ([ answerer.Name ] |> Unanswered |> Complete)

    let initialQuiz =
        { RunningQuiz.identity with
            CurrentQuizzer = (Some answerer.Name)
            CompetitionStyle = RunningCompetitionStyle.Individuals [ answerer ] }
        |> insertCurrentAnswer previouslyUnanswered

    let result =
        updateQuiz answerer.Name initialQuiz

    Assert.True(result |> Result.isOk)

[<Fact>]
let ``Given a quizzer has answered correctly from a different team for an Individual quiz When a differen quizzer answers correctly then revert the previous answerer's score``
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

    let initialQuiz =
        RunningQuiz.identity
        |> function
            | quiz -> { quiz with CompetitionStyle = RunningCompetitionStyle.Individuals [ quizzer; previousAnswerer ] }
        |> insertCurrentAnswer alreadyAnsweredQuestion

    let expectedScore =
                previousAnswerer.Score
                |> QuizScore.revertCorrectAnswer

    let revertedScoreResult =
        result {
            let! result = updateQuiz quizzer.Name initialQuiz

            let revertedScore =
                match result.QuizState.CompetitionStyle with
                | RunningCompetitionStyle.Team _ -> None
                | RunningCompetitionStyle.Individuals quizzerStates ->
                    quizzerStates
                    |> List.tryFind (QuizzerState.isQuizzer previousAnswerer.Name)
                |> Option.map (fun q -> q.Score)

            return revertedScore
        }

    Assert.Equal(Ok (Some expectedScore), revertedScoreResult)
