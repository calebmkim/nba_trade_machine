type state =
  | Welcome
  | Teams
  | Roster of (string * bool)
  | Team_transition of string
  | Player of (string * bool)
  | Player_transition of string
  | FinalTeams
  | TradeResults
  | AlteredRoster of (string * string list * string list)
  | Error of (string * state)