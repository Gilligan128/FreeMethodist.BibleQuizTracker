module FreeMethodist.BibleQuizTracker.Server.Events_Workflow

open FreeMethodist.BibleQuizTracker.Server.Workflow


type QuizzerNoLongerParticipating = { Quizzer: Quizzer; Quiz: QuizCode }

type TeamScoreChanged =
    { Quiz: QuizCode
      Team: TeamPosition
      NewScore: QuizScore }


type CurrentQuestionChanged =
    { Quiz: QuizCode
      NewQuestion: QuestionNumber }

type QuizzerParticipating = { Quizzer: Quizzer; Quiz: QuizCode }

type CurrentQuizzerChanged =
    { Quiz: QuizCode
      CurrentQuizzer: Quizzer option }

type IndividualScoreChanged =
    { Quiz: QuizCode
      Quizzer: Quizzer
      NewScore: QuizScore
      Question: QuestionNumber }

type QuizStateChanged = { Quiz: QuizCode; NewState : string }

type QuizzerPrejumped = { Quiz : QuizCode; Quizzer : Quizzer }

type RunQuizEvent =
    | QuizzerNoLongerParticipating of QuizzerNoLongerParticipating
    | TeamScoreChanged of TeamScoreChanged
    | CurrentQuestionChanged of CurrentQuestionChanged
    | QuizzerParticipating of QuizzerParticipating
    | CurrentQuizzerChanged of CurrentQuizzerChanged
    | IndividualScoreChanged of IndividualScoreChanged
    | QuizStateChanged of QuizStateChanged
    | QuizzerPrejumped of QuizzerPrejumped
