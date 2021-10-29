type trade_map
(**the trade_map type represents the "trade_map" for the current trade.
   The trademap consists of a way of organizing the trade into the teams
   involved, and which respective players each team is receiving. *)

val to_assoc_trademap : trade_map -> (string * string list) list
(**[to_assoc_trademap tmap] is the association list corresponding to
   [tmap]. So each element of the list is a tuple
   [(team, players_recieving)], in which [team] is the name of the team
   invovled in the trade and [players_recieving] is the list of players
   [team] will be recieving. Of course, the [players_receiving] will be
   made up of: a) players not on [team] but from b) on teams in [tmap] *)

val teams_in_trade : trade_map -> string list
(**[teams_int_trade tmap] is the list of teams involved in the trade*)

val make_trade_map : string list -> trade_map
(**[make_trade_map lst] is the trademap corresponding to each team in
   [lst] being involved in the trade, and each team recieving zero
   players so fa. Requires: [lst] is made up of only valid NBA team
   names. *)

val get_valid_destinations : string -> trade_map -> string list
(**[get_valid_destinations player tmap] is the list of valid destnation
   for [player] given [tmap]. Eac team in the list must be a team in
   [tmap] but not be [player]'s current team. *)

val add_player_to_trade : string -> string -> trade_map -> trade_map
(**[add_player_to_trade player team tmap] is the trademap corresponding
   to [tmap], except that [team] will now be recieving [player]. If
   [team] is already recieivng [player] then, obviously we do not add
   that [player] to the [tmap] again. *)

val valid_trade : trade_map -> bool
(**[valid_trade] checks if the trade is valid by making sure each team
   is recieiving at least 1 player. *)

val get_all_strings : trade_map -> string list
(**[get_all_strings tmap] is the list of all teams and players currently
   in [tmap]. This is mainly used ot determine the spacing of buttons in
   our screen. *)

val change_rosters :
  trade_map -> (string * string list * string list) list
(**[change_rosters tmap] is the lst in which each element corresponds to
   a team in the trademap. Each element is a tuple
   [(team, keeping, incoming)] in which [team] is the team name,
   [keeping] is the list of previous players that the team is keeping,
   and [incoming] is the list of new players the team is bringin ing. *)
