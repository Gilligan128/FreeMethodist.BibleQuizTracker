module FreeMethodist.BibleQuizTracker.Server.Tournament

type TournamentInfo = {
    Name : string option
    Church : string option
    Room : string option
    Round : string option
}
type TournamentLink =
    | Id of string
    | Info of TournamentInfo

[<RequireQualifiedAccess>]
module TournamentInfo =
    let empty =
        { Name = None; Church = None; Room = None; Round = None }