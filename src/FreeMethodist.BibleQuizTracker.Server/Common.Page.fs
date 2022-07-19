module FreeMethodist.BibleQuizTracker.Server.Common_Page

type AsyncOperationStatus<'started, 'finished> =
    | Started of 'started
    | Finished of 'finished