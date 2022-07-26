module FreeMethodist.BibleQuizTracker.Server.CreateQuizForm

open FreeMethodist.BibleQuizTracker.Server
open FreeMethodist.BibleQuizTracker.Server.Common_Page
open FreeMethodist.BibleQuizTracker.Server.Workflow


module CreateQuizForm =
 type ModalForm<'T> = | Inert
                      | Active of 'T
                      | Submitting 

 type CreateQuizFormData = {
     Code : QuizCode
     
 }
 
 type TeamCompetition = { TeamOneName: NonEmptyString; TeamTwoName: NonEmptyString }
 type CompetitionStyle = | Individual
                         | Team of TeamCompetition
 
 type Model = ModalForm<CreateQuizFormData>
 
 type Message = | Submit of AsyncOperationStatus<unit, Result<unit, unit>>
                | SetCode of string
                | SetCompetitionStyle of CompetitionStyle
                | SetTeamOneName of string
                | SetTeamTwoName of string
                