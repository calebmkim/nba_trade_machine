open Graphics
open Gameplay
open States
open Button
open Main_pages
open Transition
open WelcomeScreen

let rec show_screen cur_state trade_map =
  match cur_state with
  | Welcome ->
      let st = show_welcome Welcome in
      show_screen st trade_map
  | Teams ->
      let st = show_team_list (teams_in_trade trade_map) in
      show_screen st trade_map
  | Roster (team, are_teams_picked) ->
      let st = show_roster team are_teams_picked in
      show_screen st trade_map
  | Team_transition team_name ->
      let st, mp = team_transition team_name trade_map in
      show_screen st mp
  | FinalTeams ->
      let st = show_final_teams trade_map in
      show_screen st trade_map
  | Player x ->
      let st =
        if snd x then show_player_trade (fst x) trade_map
        else show_player (fst x)
      in
      show_screen st trade_map
  | Player_transition player_name ->
      let st, tm = player_transition player_name trade_map in
      show_screen st tm
  | TradeResults ->
      let st = show_trade_results trade_map in
      show_screen st trade_map
  | AlteredRoster team_name ->
      let st = show_altered_roster team_name trade_map in
      show_screen st trade_map

let main () =
  let _ = show_screen Welcome [] in
  ()

(* Execute the game engine. *)
let () = main ()
