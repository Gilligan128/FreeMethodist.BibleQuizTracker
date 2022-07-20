module FreeMethodist.BibleQuizTracker.Server.AnswerCorrectly_Workflow

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow

module AnswerCorrectly =

    type Data = unit

    type Command = WithinQuizCommand<Data>

    type Error =
        | QuizStateError of QuizStateError
        | QuizzerNotFound of Quizzer
        | DuplicateQuizzer of Quizzer
        | NoCurrentQuizzer
        | QuizzerAlreadyAnsweredCorrectly of QuizQuestion.QuizzerAlreadyAnsweredCorrectly

    type Event =
        | IndividualScoreChanged of IndividualScoreChanged
        | CurrentQuestionChanged of CurrentQuestionChanged
        | TeamScoreChanged of TeamScoreChanged

    type Workflow = Command -> AsyncResult<Event list, Error>
