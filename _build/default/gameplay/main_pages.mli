open Trademap
open State

val show_roster : string -> bool -> state
(**show_team_roster [team_name are_teams_picked] is the state that the
   game should be in, given the user's next mouseclick and whether
   [are_teams_picked] or not. While waiting for the user's click, it
   displays the roster of [team_name]. *)

val show_team_list : string list -> state
(**[show_team_list teams_in_trade]is the state that the game should be
   in, given the user's next mouseclick. While waiting for the
   mouseclick, It displays all 30 NBA teams, as well as displaying the
   teams the user has currentlly selected to be in the trade, as
   identified by [teams_in_trade]. Finally it displays a button allowing
   the user to go to finalize, or lock in, the teams that are currently
   selected to be in the trade. *)

val show_final_teams : trade_map -> state
(**[show_final_teams tmap] is the state that the game should be in,
   given the user's next mouseclick. While waiting for the user's
   mouseclick, it also displays the list of teams in the trade, and
   which players each team is receiving (according to [tmap]). *)

val show_player_trade : string -> trade_map -> state * trade_map
(** [show_player trade setting trade_map] is the state that the game
    should be in given the user's next mouseclick, and the current
    [setting] and [trade_map]. It also shows the player's name, along
    with his stats. It also displays a button that allows the user to
    trade the player identified in the [identifier] field of the
    [setting]*)

val show_player : string -> state
(** [show_player player_name] is the state that the game should be in
    given the user's next mouseclick. While waiting for the user's
    mouseclick, it also displays [player_name], along with his stats. *)

val show_trade_results : trade_map -> state
(** [show_trade_results tmap] is the state that the game should be in
    given the user's next mouseclick. While waiting for the user's
    mouseclick, it displays the names of the teams involved in the
    trade. *)

val show_altered_roster : string * string list * string list -> state
(** [show_altered_roster (team, remaining, incoming)] is the state that
    the team should be in after the user's nexst mouselick. While
    waiting for the mouseclick it displays the [team] along with the new
    roster, made up of the [reamining] and [incoming] players. The
    [reamining] players are shown in black and the [incoming] players
    are shown in red. *)

val show_error_message : string -> unit
