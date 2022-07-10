module FreeMethodist.BibleQuizTracker.Server.QuizHub

open System.Threading
open FreeMethodist.BibleQuizTracker.Server.MoveQuestion_Workflow
open FreeMethodist.BibleQuizTracker.Server.OverrideTeamScore.Api
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi
open FreeMethodist.BibleQuizTracker.Server.QuizzingDomain
open Microsoft.AspNetCore.SignalR
open Microsoft.FSharp.Control
open QuizzingDomain
open System.Threading.Tasks

//Used for event notification to clients

type Client =
    abstract member EnteredQuiz: QuizzerEntered -> Task
    abstract member Jumped: Quizzer -> Task
    abstract member QuestionChanged: QuestionChanged -> Task

    abstract member TeamScoreChanged: TeamScoreChanged -> Task

type Hub() =
    inherit Hub<Client>()

    member this.EnterQuiz(msg: QuizzerEntered) =
        task {
            this.Groups.AddToGroupAsync(this.Context.ConnectionId, msg.Quiz) |> ignore
            this.Clients.Group(msg.Quiz).EnteredQuiz(msg) |> ignore
        }
        
     member this.TeamScoreChanged( msg: TeamScoreChanged) =
         this.Clients.Group(msg.Quiz).TeamScoreChanged msg

     member this.ConnectToQuiz(code: QuizCode) =
         this.Groups.AddToGroupAsync(this.Context.ConnectionId, code, CancellationToken.None)
         
     member this.SendQuestionChanged(msg: QuestionChanged) =
        this.Clients.OthersInGroup(msg.Quiz).QuestionChanged msg
 