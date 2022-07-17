module FreeMethodist.BibleQuizTracker.Server.Common_Page

type AsyncOperationStatus<'t> =
    | Started
    | Finished of 't
