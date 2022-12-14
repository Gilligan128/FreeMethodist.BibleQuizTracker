module FreeMethodist.BibleQuizTracker.Server.Capabilities_Pipeline

open FreeMethodist.BibleQuizTracker.Server.Capabilities
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline
open FreeMethodist.BibleQuizTracker.Server.Workflow

type Dependencies =
    { GetQuiz: GetQuiz
      SaveQuiz: SaveQuiz }

let private onlyQuizmastersAndScorekeepers user cap =
        match user with
        | Quizmaster -> cap
        | Scorekeeper -> cap
        | Spectator -> None
        | Quizzer _ -> None

let private onlyForCurrentQuizzer currentQuizzer originalCap =
        currentQuizzer
        |> Option.bind (fun _ -> originalCap)

let runQuizCapabilities dependencies : RunQuizCapabilityProvider =
    let addQuizzer user =
        let originalCap =
            AddQuizzer_Pipeline.addQuizzerAsync dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let removeQuizzer user =
        let originalCap =
            RemoveQuizzer_Pipeline.removeQuizzer dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let answerCorrectly user quizzerOpt =
        let originalCap =
            AnswerCorrectly_Pipeline.answerCorrectly dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForCurrentQuizzer quizzerOpt

    let answersIncorrectly user quizzerOpt =
        let originalCap =
            AnswerIncorrectly.Pipeline.answerIncorrectly dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForCurrentQuizzer quizzerOpt

    let failAppealCap user quizzerOpt =
        let originalCap =
            FailAppeal.Pipeline.failAppeal dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForCurrentQuizzer quizzerOpt

    let clearAppealCap user quizzerOpt =
        let originalCap =
            ClearAppeal.Pipeline.clearAppeal dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForCurrentQuizzer quizzerOpt

    let changeCurrentQuestionCap user =
        let originalCap =
            ChangeCurrentQuestion_Pipeline.changeCurrentQuestionAsync dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let selectQuizzerCap user =
        let originalCap =
            SelectQuizzer_Pipeline.selectQuizzer dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let completeQuizCap user =
        CompleteQuiz.Pipeline.completeQuiz dependencies.GetQuiz dependencies.SaveQuiz
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let reopenQuizCap user =
        ReopenQuiz.Pipeline.reopenQuiz dependencies.GetQuiz dependencies.SaveQuiz
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
      ReopenQuiz = reopenQuizCap }
    
let runQuizCapabilitiesForQuiz dependencies (quiz : RunningQuiz) : RunQuizCapabilityProvider =
    let addQuizzer user =
        let originalCap =
            AddQuizzer_Pipeline.addQuizzerAsync dependencies.GetQuiz dependencies.SaveQuiz
        
        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let removeQuizzer user =
        let originalCap =
            RemoveQuizzer_Pipeline.removeQuizzer dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let answerCorrectly user quizzerOpt =
        let originalCap =
            AnswerCorrectly_Pipeline.answerCorrectly dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForCurrentQuizzer quizzerOpt

    let answersIncorrectly user quizzerOpt =
        let originalCap =
            AnswerIncorrectly.Pipeline.answerIncorrectly dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForCurrentQuizzer quizzerOpt

    let failAppealCap user quizzerOpt =
        let originalCap =
            FailAppeal.Pipeline.failAppeal dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForCurrentQuizzer quizzerOpt

    let clearAppealCap user quizzerOpt =
        let quizStateEnabled =
            quiz.CurrentQuizzer
            |> Option.map (fun currentQuizzer -> if quiz.Questions[quiz.CurrentQuestion].FailedAppeals |> List.contains currentQuizzer then Some currentQuizzer else None)
        let originalCap =
            ClearAppeal.Pipeline.clearAppeal dependencies.GetQuiz dependencies.SaveQuiz
        quizStateEnabled
        |> Option.map (fun _ -> originalCap)
        |> onlyQuizmastersAndScorekeepers user
        |> onlyForCurrentQuizzer quizzerOpt

    let changeCurrentQuestionCap user =
        let originalCap =
            ChangeCurrentQuestion_Pipeline.changeCurrentQuestionAsync dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let selectQuizzerCap user =
        let originalCap =
            SelectQuizzer_Pipeline.selectQuizzer dependencies.GetQuiz dependencies.SaveQuiz

        originalCap
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let completeQuizCap user =
        CompleteQuiz.Pipeline.completeQuiz dependencies.GetQuiz dependencies.SaveQuiz
        |> Some
        |> onlyQuizmastersAndScorekeepers user

    let reopenQuizCap user =
        ReopenQuiz.Pipeline.reopenQuiz dependencies.GetQuiz dependencies.SaveQuiz
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
      ReopenQuiz = reopenQuizCap }