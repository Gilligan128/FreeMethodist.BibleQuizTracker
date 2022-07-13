module FreeMethodist.BibleQuizTracker.Server.SelectQuizzer_Workflow

open FreeMethodist.BibleQuizTracker.Server.Workflow


type CurrentQuizzerChanged = { Quiz: QuizCode; Quizzer: Quizzer  }

module SelectQuizzer =
    type Input = { Quizzer: Quizzer }
    type Command = Input WithinQuizCommand
    type Error =
        | QuizState of QuizStateError
        | QuizzerNotParticipating of Quizzer
        | QuizzerAlreadyCurrent
    type Workflow = Command -> Result<CurrentQuizzerChanged, Error>     