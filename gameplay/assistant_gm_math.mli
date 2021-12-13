open Trademap

type attribute =
  | ThreePoint
  | Rebound
  | Defense
  | Playmaking
  | Scoring
  | Athleticism

exception NoMatch of string

val trade_player : string -> string list -> trade_map
(**[trade_player p att_list] creates a fair proposal for player [p]
   using based on the team needs identified by [att_list]*)
