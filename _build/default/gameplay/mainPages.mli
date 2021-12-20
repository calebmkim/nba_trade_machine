(** Functions that include general funcitonalities regarding the system,
    along with other functions that show screens relating to the Trade
    Machine Function.*)

open Trademap

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
      (**The type [state] represents which stage the user is at in terms
         of executing the trade when the system is using the Trade
         Machine feature, and therefore determines what to show the user
         on the screen. *)

type gmstate =
  | GMTeams
  | GMRoster of string
  | GMAttributes of string
  | GMRecommendation of string
  | GMError of (string * gmstate)
      (**The type [state] represents which stage the user is at in terms
         of executing the trade when the system is using the Assistant
         GM feature, and therefore determines what to show the user on
         the screen. *)

val start_state : int -> unit
(**[start_state y] opens the window and moves the graph to (0,y)*)

val show_welcome : unit -> string
(**[show_welcome a] is the state the game should be in given the user's
   click. While waiting for the user's click, it displays a welcome
   screen message. *)

val show_roster : string -> bool -> state
(**show_team_roster [team_name are_teams_picked] is the state that the
   game should be in, given the user's next mouseclick and whether
   [are_teams_picked] or not. While waiting for the user's click, it
   displays the roster of [team_name]. *)

val show_team_list : string list -> state * bool
(**[show_team_list teams_in_trade]is the state that the game should be
   in, given the user's next mouseclick. While waiting for the
   mouseclick, It displays all 30 NBA teams, as well as displaying the
   teams the user has currentlly selected to be in the trade, as
   identified by [teams_in_trade]. Finally it displays buttons allowing
   the user to go to finalize, the teams that are currently selected to
   be in the trade, or go back to the home screen *)

val show_final_teams : trade_map -> state * bool
(**[show_final_teams tmap] is the state that the game should be in,
   given the user's next mouseclick. While waiting for the user's
   mouseclick, it also displays the list of teams in the trade, and
   which players each team is receiving (according to [tmap]). There is
   also another button displayed which allows the user to go back to the
   team-selecting part of the trade machine. *)

val show_player_trade : string -> trade_map -> state * trade_map
(** [show_player_trade name trade_map] is the state that the game should
    be in given the user's next mouseclick, and the current . It also
    shows the player's name [name], along with his stats. It also
    displays a button that allows the user to trade the player
    identified in the [identifier] field of the [setting]*)

val show_player : string -> state
(** [show_player player_name] is the state that the game should be in
    given the user's next mouseclick. While waiting for the user's
    mouseclick, it also displays [player_name], along with his stats. *)

val show_trade_results : trade_map -> state
(** [show_trade_results tmap] is the state that the game should be in
    given the user's next mouseclick. While waiting for the user's
    mouseclick, it displays the names of the teams involved in the
    trade, along with the analysis of how each team fared in the trade.
    It also lets the user know if the trade would be feasible under NBA
    salary cap rules. *)

val show_altered_roster : string * string list * string list -> state
(** [show_altered_roster (team, remaining, incoming)] is the state that
    the [team] should be in after the user's nexst mouselick. While
    waiting for the mouseclick it displays the [team] along with the new
    roster, made up of the [reamining] and [incoming] players. The
    [reamining] players are shown in black and the [incoming] players
    are shown in red. *)

val show_error_message : string -> unit
(** [show_error_message s] simply shows a screen with message [s], and
    waits for a user's click. *)
