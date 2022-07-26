module FreeMethodist.BibleQuizTracker.Server.Common_Page

open Bolero

type AsyncOperationStatus<'started, 'finished> =
    | Started of 'started
    | Finished of 'finished
    
/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/quiz">] Quiz of quizCode: string