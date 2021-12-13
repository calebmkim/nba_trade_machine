(** Functions for recommending a trade when using the Assistant GM
    feature*)

open Trademap

exception NoMatch of string

val trade_player : string -> string list -> trade_map
(**[trade_player p att_list] is a fair trade proposal for player [p]
   using based on the team needs identified by [att_list]*)
