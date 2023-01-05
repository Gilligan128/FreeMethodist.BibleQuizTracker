module FreeMethodist.BibleQuizTracker.Server.Tournament

type TournamentLink =
    | Id of string
    | Name of string

type TournamentInfo = {
    Link : TournamentLink option
    Church : string option
    Room : string option
    Round : string option
}


[<RequireQualifiedAccess>]
module TournamentInfo =
    let empty =
        { Link  = None ; Church = None; Room = None; Round = None }