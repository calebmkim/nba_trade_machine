open Graphics
open Gameplay
open Trademap
open Button
open Main_pages
open Transition
open WelcomeScreen
open State

let rec show_screen cur_state trade_map =
  match cur_state with
  | Welcome ->
      let st = show_welcome () in
      show_screen st trade_map
  | Teams ->
      let st = show_team_list (teams_in_trade trade_map) in
      show_screen st trade_map
  | Roster (team, are_teams_picked) ->
      let st = show_roster team are_teams_picked in
      show_screen st trade_map
  | Team_transition team_name ->
      let st, tmap = team_transition team_name trade_map in
      show_screen st tmap
  | FinalTeams ->
      let st = show_final_teams trade_map in
      show_screen st trade_map
  | Player (name, in_final_version) ->
      if in_final_version then
        let st, tmap = show_player_trade name trade_map in
        show_screen st tmap
      else
        let st = show_player name in
        show_screen st trade_map
  | Player_transition player_name ->
      let st, tmap = player_transition player_name trade_map in
      show_screen st tmap
  | TradeResults ->
      let st = show_trade_results trade_map in
      show_screen st trade_map
  | AlteredRoster team_name ->
      let st = show_altered_roster team_name in
      show_screen st trade_map
  | Error (msg, prev_state) ->
      let _ = show_error_message msg in
      show_screen prev_state trade_map

let main () =
  let _ = show_screen Welcome (make_trade_map []) in
  ()

(* Execute the game engine. *)
let () = main ()
