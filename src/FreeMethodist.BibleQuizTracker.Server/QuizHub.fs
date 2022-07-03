module FreeMethodist.BibleQuizTracker.Server.QuizHub

open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Api
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain
open Microsoft.AspNetCore.SignalR
open QuizzingDomain
open System.Threading.Tasks

//Used for event notification to clients

type Client =
    abstract member EnteredQuiz: QuizzerEntered -> Task
    abstract member Jumped: Quizzer -> Task
    abstract member HandleQuizEvent: 'T -> Task


type Hub() =
    inherit Hub<Client>()

    member this.EnterQuiz(msg: QuizzerEntered) =
        task {
            this.Groups.AddToGroupAsync(this.Context.ConnectionId, msg.Quiz)
            this.Clients.Group(msg.Quiz).EnteredQuiz(msg)
        }
        
     member this.TeamScoreChanged( msg: TeamScoreChanged) =
         this.Clients.Group(msg.Quiz).HandleQuizEvent msg

     member this.ConnectToQuiz(code: QuizCode) =
         this.Groups.AddToGroupAsync(this.Context.ConnectionId, code)