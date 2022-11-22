module FreeMethodist.BibleQuizTracker.Server.Tests.UpdatesQuizScore

open FreeMethodist.BibleQuizTracker.Server.Workflow
open Xunit

[<Fact>]
let ``All Quizzers on a team get new scores`` () =
    let participantsOne =
        [ "John"; "Jim"; "Joan" ]

    let participantsTwo =
        [ "Jamie"; "Jorge"; "Jam" ]

    let quiz =
        RunningQuiz.newTeamQuiz
        |> Arrange.withParticipants (participantsOne |> List.map QuizzerState.create)
        |> Arrange.withTeamTwoParticipants (participantsTwo |> List.map QuizzerState.create)
        |> fun quiz ->
            let questions =
                participantsOne @ participantsTwo
                |> List.mapi (fun index quizzer ->
                    index+1 |> PositiveNumber.numberOrOne,
                    (CompletedAnswer.Answered
                        { Answerer = quizzer
                          IncorrectAnswerers = [] }
                     |> QuizAnswer.Complete
                     |> QuestionState.create))

            { quiz with Questions = questions |> Map.ofList }

    let scoredQuiz = Score.updateQuizScores quiz

    match scoredQuiz.CompetitionStyle with
    | RunningCompetitionStyle.Individuals _ -> failwith "shouldn't be Individuals"
    | RunningCompetitionStyle.Team (teamOne, teamTwo) ->
        teamOne.Quizzers @ teamTwo.Quizzers
        |> Seq.iter (fun quizzer -> Assert.Equal(QuizScore.zero |> QuizScore.correctAnswer, quizzer.Score))
