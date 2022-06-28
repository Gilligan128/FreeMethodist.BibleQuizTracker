module FreeMethodist.BibleQuizTracker.Server.EnterQuizPipeline

open System
open FreeMethodist.BibleQuizTracker.Server.QuizzingApi

type ValidEntrance = {
    Quizzer: Quizzer
    Team: TeamName
    Participation: ParticipationType
    Timestamp: DateTimeOffset
}
type ValidateEntrance = QuizCode -> UnvalidatedEntrance -> Result<ValidEntrance, EnterQuizError>
type ReportEntrance = QuizCode -> ValidEntrance -> QuizzerEntered