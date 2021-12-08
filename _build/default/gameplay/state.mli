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

type gmstate =
  | GMTeams
  | GMRoster of string
  | GMAttributes of string
  | GMRecommendation of string
  | GMError of (string * gmstate)

(**The type state represents which stage the user is at in terms of
   executing the trade and therefore determines what to show the user on
   the screen. *)
