module FreeMethodist.BibleQuizTracker.Server.Events_Workflow

 open FreeMethodist.BibleQuizTracker.Server.Workflow

 
 type QuizzerNoLongerParticipating = { Quizzer : Quizzer; Quiz: QuizCode }
 
 type TeamScoreChanged = {
    Quiz: QuizCode
    Team: TeamPosition
    NewScore: TeamScore
 }
 
 type CurrentQuestionChanged = { Quiz: QuizCode; NewQuestion: QuestionNumber; }
 
 type QuizzerParticipating = { Quizzer: Quizzer; Quiz: QuizCode }
 
 type CurrentQuizzerChanged = { Quiz: QuizCode; Quizzer: Quizzer  }

 
 type RunQuizEvent =
     | QuizzerNoLongerParticipating of QuizzerNoLongerParticipating
     | TeamScoreChanged of TeamScoreChanged
     | CurrentQuestionChanged of CurrentQuestionChanged
     | QuizzerParticipating of QuizzerParticipating
     | CurrentQuizzerChanged of CurrentQuizzerChanged