(** Data structure to handle trades between NBA teams*)

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
(**[teams_int_trade tmap] is the list of teams involved in the trade
   according to [tmap]*)

val make_trade_map : string list -> trade_map
(**[make_trade_map lst] is the trademap corresponding to each team in
   [lst] being involved in the trade, and each team recieving zero
   players so far. Requires: [lst] is made up of only valid NBA team
   names. *)

val get_valid_destinations : string -> trade_map -> string list
(**[get_valid_destinations player tmap] is the list of valid destnation
   for [player] given [tmap]. Each team in the list must be a team in
   [tmap] but not be [player]'s current team. *)

val add_player_to_trade : string -> string -> trade_map -> trade_map
(**[add_player_to_trade player team tmap] is the trademap corresponding
   to [tmap], except that [team] will now be recieving [player]. If
   [team] is already recieivng [player] then, obviously we do not add
   that [player] to the [tmap] again. *)

val valid_trade : trade_map -> bool
(**[valid_trade] is [true] if the trade is valid (i.e each team is
   recieiving at least 1 player). [false] otherwise. *)

val get_all_strings : trade_map -> string list
(**[get_all_strings tmap] is the list of all teams and players currently
   in [tmap]. This is mainly used ot determine the spacing of buttons in
   our screen. *)

val change_rosters :
  trade_map -> (string * string list * string list) list
(**[change_rosters tmap] is the lst in which each element corresponds to
   a team in the [tmap]. Each element is a tuple
   [(team, keeping, incoming)] in which [team] is the team name,
   [keeping] is the list of previous players that the team is keeping
   (this will be very close to the current roster usually), and
   [incoming] is the list of new players the team is bringing in. *)

val players_losing : string -> trade_map -> string list
(** [players_losing team_name tmap] is the list of players [team_name]
    is losing given trademap [tmap]. Requires: [team] is a team in the
    trade indicated by [tmap]*)

val players_acquiring : string -> trade_map -> string list
(** [player_acquiring team_name t_map] is the list of players
    [team_name] is acquiring given trademap [t_map]. Requires: [team] is
    a team in the trade indicated by [tmap]*)

val is_team_in_trade : string -> trade_map -> bool
(** [is_team_in_trade team tmap] is [true] if [team] is in [tmap] and
    [false] otherwise*)

val add_team_to_trade : string -> trade_map -> trade_map
(** [add_team_to_trade team tmap] adds [team] to [tmap]. If [team] is
    already in [tmap] then simply return [tmap]*)

val remove_team_from_trade : string -> trade_map -> trade_map
(** [remove_team_from_trade team tmap] removes [team] from [tmap]. If
    [team] is not in [tmap] then simply return [tmap]*)

val remove_player_from_trade : string -> trade_map -> trade_map
(** [remove_player_from_trade] removes [player] from [tmap]. If [player]
    is not in [tmap] then simply return [tmap]*)

val is_player_in_trade : string -> trade_map -> bool
(** [is_player_in_trade player tmap] is [true] if [player] is in [tmap].
    If [player] is not in [tmap] then [false]*)
