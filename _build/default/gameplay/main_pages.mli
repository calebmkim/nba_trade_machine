open States

val show_team_roster :
  setting -> bool -> (string * string list) list -> state
(**show_team_roster [settings teams_picked trade_map] is the state that
   the game should be in, given the user's next mouseclick and current
   [trade map], [settings] amd whether [teams_picked] or not. At the
   same time, it displays the roster of the NBA team as indicated by the
   identifier in [setting]. *)

val show_team_list : string list -> state
(**[show_team_list lst]is the state that the game should be in, given
   the user's next mouseclick, as well as the current list of teams in
   the trade [lst]. It displays all 30 NBA teams, as well a banner that
   reads teams currently in trade, which displays the teams the user has
   currentlly selected to be in the trade, as identified by [lst].
   Finally it displays a button allowing the user to go to finalize, or
   lock in, the teams that are currently selected to be in the trade. *)

val show_final_teams :
  (string * string list) list -> state * (string * string list) list
(**[show_final_teams trade_map] is the tuple [(st, tm)] in which st is
   the state that the game should be in, given the user's next
   mouseclick and the [trade_map]. [tm] is the current trade_map for the
   game. It also displays the list of teams, and which players each team
   is receiving (according to [trade_map] on the graph. *)

val show_player_trade : setting -> (string * string list) list -> state
(** [show_player trade setting trade_map] is the state that the game
    should be in given the user's next mouseclick, and the current
    [setting] and [trade_map]. It also shows the player's name, along
    with his stats. It also displays a button that allows the user to
    trade the player identified in the [identifier] field of the
    [setting]*)

val show_player : setting -> state
(** [show_player trade setting] is the state that the game should be in
    given the user's next mouseclick, and the given [setting]. It also
    shows the player's name, along with his stats. *)

val show_trade_results : (string * string list) list -> state
(** [show_player trade_map] is the state that the game should be in
    given the user's next mouseclick, and the given [trade_map]. It also
    shows the names of the teams involved in the trade, and if any of
    these names is clicked on, will reveal the altered roster, givne the
    [trade_map]. *)

val show_new_roster :
  string * string list * string list ->
  (string * string list) list ->
  state
(** [show_new_roster (name, old, incoming), trade_map] is the state that
    the game should be in given the user's next mouseclick, and the
    given [trade_map]. It also shows the new roster of team with name
    [name], and the new roster, with the old players (i.e the ones not
    in the trade) in black and the new players (i.e the new arrivals who
    just got traded) in red. *)
