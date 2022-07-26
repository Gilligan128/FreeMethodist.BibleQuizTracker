module FreeMethodist.BibleQuizTracker.Server.QuizHub

open System.Threading
open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open Microsoft.AspNetCore.SignalR
open Microsoft.FSharp.Control
open System.Threading.Tasks

//Used for event notification to clients

type Client =
    abstract member EnteredQuiz: QuizzerEntered -> Task
    abstract member Jumped: Quizzer -> Task
    abstract member QuestionChanged: CurrentQuestionChanged -> Task

    abstract member TeamScoreChanged: TeamScoreChanged -> Task

    abstract member RunQuizEventOccurred: RunQuizEvent -> Task

type Hub() =
    inherit Hub<Client>()

    member this.EnterQuiz(msg: QuizzerEntered) =
        task {
            this.Groups.AddToGroupAsync(this.Context.ConnectionId, msg.Quiz)
            |> ignore

            this.Clients.Group(msg.Quiz).EnteredQuiz(msg)
            |> ignore
        }

    member this.ConnectToQuiz(code: QuizCode, previousCode: QuizCode option) =
        let leaveGroup =
            previousCode
            |> Option.map (fun code ->
                this.Groups.RemoveFromGroupAsync(this.Context.ConnectionId, code, CancellationToken.None))
            |> Option.map Async.AwaitTask

        this.Groups.AddToGroupAsync(this.Context.ConnectionId, code, CancellationToken.None)
        |> Async.AwaitTask
        |> Some
        |> Option.bind (fun innerTask ->
            leaveGroup
            |> Option.map (Async.bind (fun _ -> innerTask)))
    
    member this.DisconnectFromQuiz(previousCode: QuizCode) =
         this.Groups.RemoveFromGroupAsync(this.Context.ConnectionId, previousCode, CancellationToken.None)
    
    member this.SendRunQuizEventOccurred (quiz: QuizCode) (msg: RunQuizEvent) =
        this
            .Clients
            .OthersInGroup(quiz)
            .RunQuizEventOccurred msg
