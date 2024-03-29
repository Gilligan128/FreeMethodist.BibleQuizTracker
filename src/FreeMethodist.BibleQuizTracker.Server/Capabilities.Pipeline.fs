﻿module FreeMethodist.BibleQuizTracker.Server.Capabilities_Pipeline

open System.Collections.Generic
open FreeMethodist.BibleQuizTracker.Server.Capabilities
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows
open FreeMethodist.BibleQuizTracker.Server.ClearAppeal.Pipeline
open FreeMethodist.BibleQuizTracker.Server.FailAppeal.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Prejump_Pipeline

type Dependencies =
    { GetQuiz: GetQuiz
      SaveQuiz: SaveQuiz }

let private onlyQuizmastersAndScorekeepers user cap =
        match user with
        | Quizmaster -> cap
        | Scorekeeper -> cap
        | Spectator -> None
        | Quizzer _ -> None

let private onlyForQuizzer currentQuizzer originalCap =
        currentQuizzer
        |> Option.bind (fun _ -> originalCap)

let runQuizCapabilitiesForQuiz dependencies : RunQuizCapabilityForQuizProvider =
    let runQuizWorkflowEngine workflow mapErrors =
        runQuizWorklfowEngine dependencies.GetQuiz dependencies.SaveQuiz workflow mapErrors
    
    let addQuizzer (quiz : RunningQuiz) user =
        let originalCap input =
            AddQuizzer_Pipeline.addQuizzerAsync dependencies.GetQuiz dependencies.SaveQuiz { Quiz = quiz.Code; Data = input }
        
        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let removeQuizzer (quiz : RunningQuiz) user =
        let originalCap input =
            RemoveQuizzer_Pipeline.removeQuizzer dependencies.GetQuiz dependencies.SaveQuiz { Quiz = quiz.Code; Data = input }

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let answerCorrectly (quiz : RunningQuiz) user =
        let originalCap () =
            AnswerCorrectly_Pipeline.answerCorrectly dependencies.GetQuiz dependencies.SaveQuiz { Quiz = quiz.Code; Data = () }

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForQuizzer quiz.CurrentQuizzer

    let answersIncorrectly (quiz : RunningQuiz) user =
        let originalCap () =
            AnswerIncorrectly.Pipeline.answerIncorrectly dependencies.GetQuiz dependencies.SaveQuiz { Quiz = quiz.Code; Data = () }

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForQuizzer quiz.CurrentQuizzer

    let failAppealCap (quiz : RunningQuiz) user =
        let originalCap () =
             runQuizWorkflowEngine failAppealPureWorkflow FailAppeal.Error.DbError {Quiz = quiz.Code; Data = ()}

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForQuizzer quiz.CurrentQuizzer

    let clearAppealCap (quiz : RunningQuiz) user  =
        let quizStateEnabled =
            quiz.CurrentQuizzer
            |> Option.map (fun currentQuizzer -> if quiz.Questions[quiz.CurrentQuestion].FailedAppeals |> List.contains currentQuizzer then Some currentQuizzer else None)
        let originalCap () =  runQuizWorkflowEngine clearAppealPureWorkflow ClearAppeal.Error.DbError {Quiz = quiz.Code; Data = ()}
           
        quizStateEnabled
        |> Option.map (fun _ -> originalCap)
        |> onlyQuizmastersAndScorekeepers user

    let changeCurrentQuestionCap (quiz : RunningQuiz) user =
        let originalCap input =
            ChangeCurrentQuestion_Pipeline.changeCurrentQuestionAsync dependencies.GetQuiz dependencies.SaveQuiz { Quiz = quiz.Code; Data = input }

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let selectQuizzerCap (quiz : RunningQuiz) user =
        let originalCap input =
            SelectQuizzer_Pipeline.selectQuizzer dependencies.GetQuiz dependencies.SaveQuiz { Quiz = quiz.Code; Data = input }

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let completeQuizCap (quiz : RunningQuiz) user =
        fun () -> CompleteQuiz.Pipeline.completeQuiz dependencies.GetQuiz dependencies.SaveQuiz quiz.Code 
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let reopenQuizCap (quiz : RunningQuiz) user =
        fun () -> ReopenQuiz.Pipeline.reopenQuiz dependencies.GetQuiz dependencies.SaveQuiz quiz.Code
        |> Some
        |> onlyQuizmastersAndScorekeepers user
    
    let prejumpCap (quiz: RunningQuiz) user =
        fun () -> (runQuizWorkflowEngine prejumpWorkflow Prejump.Error.DbError) { Quiz = quiz.Code; Data = () }
        |> Some
        |> onlyQuizmastersAndScorekeepers user
    
    { AddQuizzer = addQuizzer
      RemoveQuizzer = removeQuizzer
      AnswerCorrectly = answerCorrectly
      AnswerIncorrectly = answersIncorrectly
      FailAppeal = failAppealCap
      ClearAppeal = clearAppealCap
      ChangeCurrentQuestion = changeCurrentQuestionCap
      SelectQuizzer = selectQuizzerCap
      CompleteQuiz = completeQuizCap
      ReopenQuiz = reopenQuizCap
      Prejump = prejumpCap }