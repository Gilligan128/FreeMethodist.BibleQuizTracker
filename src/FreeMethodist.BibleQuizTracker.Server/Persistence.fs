module FreeMethodist.BibleQuizTracker.Server.Persistence

open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain


let mutable private exampleQuiz: TeamQuiz =
    let exampleResult =
        result {
            let! teamOneScore = TeamScore.create 20
            let! teamTwoScore = TeamScore.create 40
            let! jimScore = TeamScore.create 20
            let! jinaScore = TeamScore.create 40
            let! johnScore = TeamScore.create 0
            let! juniScore = TeamScore.create 0
            let! currentQuestion = PositiveNumber.create 3 "CurrentQuestion"
            return
                { Code = "Example"
                  TeamOne =
                    { Name = "LEFT"
                      Score = teamOneScore
                      Captain = None
                      Quizzers =
                        [ { Name = "Jim"
                            Score = jimScore
                            Participation = In }
                          { Name = "John"
                            Score = johnScore
                            Participation = In } ] }
                  TeamTwo =
                    { Name = "RIGHT"
                      Score = teamTwoScore
                      Captain = None
                      Quizzers =
                        [ { Name = "Jina"
                            Score = jinaScore
                            Participation = In }
                          { Name = "Juni"
                            Score = juniScore
                            Participation = In } ] }
                  CurrentQuestion = currentQuestion
                  Questions = [] }
        }

    match exampleResult with
    | Ok example -> Running example
    | Error _ -> Running RunningTeamQuiz.identity

let getQuiz: GetTeamQuiz =
    fun code -> exampleQuiz

let saveQuiz: SaveTeamQuiz =
    fun quiz ->  exampleQuiz <- quiz 
