module FreeMethodist.BibleQuizTracker.Server.QuizHub

open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open Microsoft.AspNetCore.SignalR
open QuizzingDomain
open System.Threading.Tasks

//Used for event notification to clients

type Client =
    abstract member EnteredQuiz: QuizzerEntered -> Task
    abstract member Jumped: Quizzer -> Task

type Hub () =
    inherit Hub<Client> ()
    
    member this.EnterQuiz (msg:QuizzerEntered) =
       this.Groups.AddToGroupAsync(this.Context.ConnectionId, msg.Quiz) |> ignore
       this.Clients.Group(msg.Quiz).EnteredQuiz(msg)