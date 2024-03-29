﻿namespace FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

[<RequireQualifiedAccess>]
module ChangeCurrentQuestion =

    type QuestionData = { Question: QuestionNumber }
    type Command = WithinQuizCommand<QuestionData>

    type Error =
        | QuizState of QuizStateError
        | DbError of DbError

    type Workflow = Command -> AsyncResult<CurrentQuestionChanged, Error>

[<RequireQualifiedAccess>]
module AddQuizzer =
    type Data =
        { Name: Quizzer
          Team: TeamPosition option }

    type Command = WithinQuizCommand<Data>

    type Error =
        | QuizState of QuizStateError
        | QuizzerAlreadyAdded of Quizzer
        | DbError of DbError

    type Workflow = Command -> AsyncResult<QuizzerParticipating, Error>


[<RequireQualifiedAccess>]
module RemoveQuizzer =
    type Data = { Quizzer: Quizzer }
    type Command = WithinQuizCommand<Data>

    type Error =
        | QuizStateError of QuizStateError
        | QuizzerNotParticipating of Quizzer
        | DbError of DbError

    type Event =
        | CurrentQuizzerChanged of CurrentQuizzerChanged
        | QuizzerNoLongerParticipating of QuizzerNoLongerParticipating

    type Workflow = Command -> AsyncResult<Event list, Error>

[<RequireQualifiedAccess>]
module SelectQuizzer =
    type Input = { Quizzer: Quizzer }
    type Command = Input WithinQuizCommand

    type Error =
        | QuizState of QuizStateError
        | QuizzerNotParticipating of Quizzer
        | QuizzerAlreadyCurrent
        | DbError of DbError

    type Workflow = Command -> AsyncResult<CurrentQuizzerChanged, Error>

[<RequireQualifiedAccess>]
module AnswerCorrectly =

    type Data = unit

    type Command = WithinQuizCommand<Data>

    type Error =
        | QuizStateError of QuizStateError
        | QuizzerNotFound of Quizzer
        | NoCurrentQuizzer
        | QuizzerAlreadyAnsweredCorrectly of QuizAnswer.QuizzerAlreadyAnsweredCorrectly
        | DbError of DbError

    type Event =
        | IndividualScoreChanged of IndividualScoreChanged
        | CurrentQuestionChanged of CurrentQuestionChanged
        | TeamScoreChanged of TeamScoreChanged

    type Workflow = Command -> AsyncResult<Event list, Error>

[<RequireQualifiedAccess>]
module AnswerIncorrectly =
    type Command = WithinQuizCommand<unit>

    type Event =
        | CurrentQuizzerChanged of CurrentQuizzerChanged
        | IndividualScoreChanged of IndividualScoreChanged
        | TeamScoreChanged of TeamScoreChanged

    type Error =
        | QuizState of QuizStateError
        | NoCurrentQuizzer of NoCurrentQuizzer
        | QuizzerAlreadyAnsweredIncorrectly of QuizAnswer.QuizzerAlreadyAnsweredIncorrectly
        | DbError of DbError

    type Workflow = Command -> AsyncResult<Event list, Error>

[<RequireQualifiedAccess>]
module FailAppeal =
    type Command = WithinQuizCommand<unit>

    type Error =
        | QuizState of QuizStateError
        | NoCurrentQuizzer of NoCurrentQuizzer
        | AppealAlreadyFailed of Quizzer
        | DbError of DbError

    type Event =
        | TeamScoreChanged of TeamScoreChanged
        | IndividualScoreChanged of IndividualScoreChanged

    type Workflow = Command -> AsyncResult<Event list, Error>

[<RequireQualifiedAccess>]
module ClearAppeal =
    type Command = WithinQuizCommand<unit>

    type Error =
        | QuizState of QuizStateError
        | NoFailedAppeal
        | DbError of DbError

    type Event = TeamScoreChanged of TeamScoreChanged

    type Workflow = Command -> AsyncResult<Event list, Error>

[<RequireQualifiedAccess>]
module CompleteQuiz =
    type Command = QuizCode

    type Error =
        | QuizState of QuizStateError
        | DbError of DbError

    type Event = QuizStateChanged of QuizStateChanged

    type Workflow = Command -> AsyncResult<Event list, Error>

[<RequireQualifiedAccess>]
module ReopenQuiz =
    type Command = QuizCode

    type Error =
        | QuizState of QuizStateError
        | DbError of DbError

    type Event = QuizStateChanged of QuizStateChanged
    type Workflow = Command -> AsyncResult<Event list, Error>
    
[<RequireQualifiedAccess>]
module Prejump =
    type Command = Unit
    type Error = | DbError of DbError
                 | RemoteError of string
                 | NoCurrentQuizzer of NoCurrentQuizzer
                 | WrongQuizState of QuizStateError

    type Event = | Prejump of QuizzerPrejumped
                 | TeamScoreChanged of TeamScoreChanged
                 | IndividualScoreChanged of IndividualScoreChanged
    type Workflow = Command -> AsyncResult<Event list, Error>

