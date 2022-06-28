module FreeMethodist.BibleQuizTracker.Server.QuizHub

open Microsoft.AspNetCore.SignalR
open Types
open System.Threading.Tasks

type QuizRoomCLient =
    abstract member EnteredQuiz: quizzer:Quizzer -> Task
    abstract member Jumped: Quizzer -> Task

type QuizHub () =
    inherit Hub<QuizRoomCLient> ()
    
    member this.EnterQuiz (msg:QuizCode*Quizzer) =
       let (quizCode, quizzer) = msg
       this.Groups.AddToGroupAsync(this.Context.ConnectionId, quizCode) |> ignore
       this.Clients.Group(quizCode).EnteredQuiz(quizzer)