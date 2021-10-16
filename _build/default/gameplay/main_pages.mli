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
    with his stats *)

val show_player : setting -> state

val show_trade_results : (string * string list) list -> state

val show_new_roster :
  string * string list * string list ->
  (string * string list) list ->
  state
