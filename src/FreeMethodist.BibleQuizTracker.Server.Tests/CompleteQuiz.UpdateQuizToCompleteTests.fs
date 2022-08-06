module FreeMethodist.BibleQuizTracker.Server.Tests.CompleteQuiz_UpdateQuizToCompleteTests



open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.CompleteQuiz.Pipeline

[<Fact>]
let ``When completing a quiz Then quiz is in a Completed state`` () =
    let initialQuiz =
        RunningTeamQuiz.identity
        |> fun quiz -> { quiz with Questions = Map.empty }

    let actualQuiz =
        updateQuizToComplete initialQuiz

    let expectedQuiz: CompletedQuiz =
        { Code = initialQuiz.Code
          CompetitionStyle =
            CompletedCompetitionStyle.Team(
                { Name = initialQuiz.TeamOne.Name
                  Score = initialQuiz.TeamOne.Score
                  Quizzers =
                    initialQuiz.TeamOne.Quizzers
                    |> List.map (fun q -> { Name = q.Name; Score = q.Score }) },
                { Name = initialQuiz.TeamTwo.Name
                  Score = initialQuiz.TeamTwo.Score
                  Quizzers =
                    initialQuiz.TeamTwo.Quizzers
                    |> List.map (fun q -> { Name = q.Name; Score = q.Score }) }
            )
          CompletedQuestions = [] }

    Assert.Equal(Completed expectedQuiz, Completed actualQuiz)

let private numberOf value =
    PositiveNumber.create "number" value
    |> function
        | Ok number -> number
        | Error _ -> PositiveNumber.one

[<Fact>]
let ``Given existing questions When completing a quiz Then all questions are complete`` () =

    let unansweredIncorrectAnswerers =
        [ "Juni" ]

    let unansweredQuestion: QuestionState =
        { FailedAppeal = None
          AnswerState =
            QuizAnswer.Complete
            <| Unanswered unansweredIncorrectAnswerers }

    let answeredAnswerState =
        { IncorrectAnswerers = [ "Jim" ]
          Answerer = "Jim" }

    let answeredAppealFailure = Some "Jina"

    let answeredQuestion: QuestionState =
        { FailedAppeal = answeredAppealFailure
          AnswerState =
            Answered answeredAnswerState
            |> QuizAnswer.Complete }

    let incompleteAnswerers = [ "Juni" ]

    let incompleteQuestion: QuestionState =
        { FailedAppeal = None
          AnswerState = QuizAnswer.Incomplete <| incompleteAnswerers }

    let initialQuiz =
        RunningTeamQuiz.identity
        |> fun quiz ->
            { quiz with
                Questions =
                    Map.empty
                    |> Map.add PositiveNumber.one unansweredQuestion
                    |> Map.add (numberOf 2) answeredQuestion
                    |> Map.add (numberOf 3) (incompleteQuestion) }


    let actualQuiz =
        updateQuizToComplete initialQuiz

    let actualQuestions =
        actualQuiz.CompletedQuestions

    let expectedQuestions =
        [ { FailedAppeal = None
            AnswerState = Unanswered unansweredIncorrectAnswerers }
          { FailedAppeal = answeredAppealFailure
            AnswerState = Answered answeredAnswerState }
          { FailedAppeal = None
            AnswerState = Unanswered incompleteAnswerers } ]

    Assert.Equal(expectedQuestions.Length, actualQuestions.Length)
    Assert.Equal<CompletedQuestion list>(expectedQuestions, actualQuestions)
