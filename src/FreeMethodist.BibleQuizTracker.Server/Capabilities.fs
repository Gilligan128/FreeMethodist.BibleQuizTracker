namespace FreeMethodist.BibleQuizTracker.Server.Capabilities

open FreeMethodist.BibleQuizTracker.Server.Events_Workflow
open FreeMethodist.BibleQuizTracker.Server.Workflow
open FreeMethodist.BibleQuizTracker.Server.RunQuiz.Workflows

type CompleteQuizCap = unit -> AsyncResult<CompleteQuiz.Event list, CompleteQuiz.Error>
type ReopenQuizCap = unit -> AsyncResult<ReopenQuiz.Event list, ReopenQuiz.Error>
type AnswerCorrectlyCap = (unit -> AsyncResult<AnswerCorrectly.Event list, AnswerCorrectly.Error>) 
type AnswerIncorrectlyCap = (unit -> AsyncResult<AnswerIncorrectly.Event list, AnswerIncorrectly.Error>) 
type AddQuizzerCap = (AddQuizzer.Data -> AsyncResult<QuizzerParticipating, AddQuizzer.Error>) 
type RemoveQuizzerCap = (RemoveQuizzer.Data -> AsyncResult<RemoveQuizzer.Event list, RemoveQuizzer.Error>) 
type FailAppealCap = (unit -> AsyncResult<FailAppeal.Event list, FailAppeal.Error>) 
type ClearAppealCap = (unit -> AsyncResult<ClearAppeal.Event list, ClearAppeal.Error>) 
type ChangeCurrentQuestionCap = (ChangeCurrentQuestion.QuestionData -> AsyncResult<CurrentQuestionChanged, ChangeCurrentQuestion.Error>) 
type SelectQuizzerCap = (SelectQuizzer.Input -> AsyncResult<CurrentQuizzerChanged, SelectQuizzer.Error>) 

type RunQuizCapabilityProvider =
    { AddQuizzer: User -> AddQuizzer.Workflow option
      RemoveQuizzer: User -> RemoveQuizzer.Workflow option
      AnswerCorrectly: User -> Quizzer option -> AnswerCorrectly.Workflow option
      AnswerIncorrectly: User -> Quizzer option -> AnswerIncorrectly.Workflow option
      FailAppeal: User -> Quizzer option -> FailAppeal.Workflow option
      ClearAppeal: User -> Quizzer option -> ClearAppeal.Workflow option
      ChangeCurrentQuestion: User -> ChangeCurrentQuestion.Workflow option
      SelectQuizzer: User -> SelectQuizzer.Workflow option
      CompleteQuiz: User -> CompleteQuiz.Workflow option
      ReopenQuiz: User -> ReopenQuiz.Workflow option }

type RunQuizCapabilityForQuizProvider =
    { AddQuizzer: RunningQuiz -> User -> AddQuizzerCap option
      RemoveQuizzer: RunningQuiz -> User -> RemoveQuizzerCap option
      AnswerCorrectly: RunningQuiz -> User -> AnswerCorrectlyCap option
      AnswerIncorrectly: RunningQuiz -> User  -> AnswerIncorrectlyCap option
      FailAppeal: RunningQuiz -> User -> FailAppealCap option
      ClearAppeal: RunningQuiz -> User  -> ClearAppealCap option
      ChangeCurrentQuestion: RunningQuiz -> User -> ChangeCurrentQuestionCap option
      SelectQuizzer: RunningQuiz -> User -> SelectQuizzerCap option
      CompleteQuiz: RunningQuiz -> User -> CompleteQuizCap option
      ReopenQuiz: RunningQuiz -> User -> ReopenQuizCap option }

type ProvideCapabilitiesForQuiz = RunningQuiz -> RunQuizCapabilityProvider