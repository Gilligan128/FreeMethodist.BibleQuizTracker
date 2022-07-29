module FreeMethodist.BibleQuizTracker.Server.Common_Page

open Bolero
open FreeMethodist.BibleQuizTracker.Server.Workflow

type AsyncOperationStatus<'started, 'finished> =
    | Started of 'started
    | Finished of 'finished
    
/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/quiz/{quizCode}/run">] QuizRun of quizCode: string
    | [<EndPoint "/quiz/{quizCode}/spectate">] QuizSpectate of quizCode: string
    
    
let mapDbErrorToString error =
    match error with
    | Exception exn -> exn.Message
    | DbError.RemoteError message -> message
