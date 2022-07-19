module FreeMethodist.BibleQuizTracker.Server.JumpPipeline

open System
open FreeMethodist.BibleQuizTracker.Server.Workflow

//Jump pipeline
type ValidJump =
    { Quizzer: Quizzer
      Timestamp: DateTimeOffset }

type Jump =
    | UnvalidatedJump
    | ValidJump

//Jump Pipeline steps
type ValidateJump = JumpCommand -> Result<ValidJump, JumpError>
type GetExistingJumps = QuizCode -> ValidJump list
type CalculateJumpOrder = ValidJump list -> QuizCode -> ValidJump -> JumpOrderChanged
