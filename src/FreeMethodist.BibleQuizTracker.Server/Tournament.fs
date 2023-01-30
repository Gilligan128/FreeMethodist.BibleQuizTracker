module FreeMethodist.BibleQuizTracker.Server.Tournament

type TournamentLink =
    | Id of string
    | Name of string

type GradeDivision =
    | SeniorTeen
    | YoungTeen
    | Kids
    | QUIC
    | Custom of string
    
type CompetitionDivision =
    | Veteran
    | Rookie

type TournamentInfo = {
    Link : TournamentLink option
    Church : string option
    Room : string option
    Round : string option
    GradeDivision : GradeDivision option
    CompetitionDivision : CompetitionDivision option
}


[<RequireQualifiedAccess>]
module TournamentInfo =
    let empty =
        { Link  = None ; Church = None; Room = None; Round = None ; GradeDivision = None; CompetitionDivision = None }