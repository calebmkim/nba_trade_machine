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
  | Roster x ->
      let st = show_team_roster (fst x) (snd x) trade_map in
      show_screen st trade_map
  | Teams x ->
      let st = show_team_list x in
      show_screen st trade_map
  | Team_transition x ->
      let st = team_options x in
      show_screen st trade_map
  | FinalTeams x ->
      let st, tm = show_final_teams x in
      show_screen st tm
  | Player x ->
      let st =
        if snd x then show_player_trade (fst x) trade_map
        else show_player (fst x)
      in
      show_screen st trade_map
  | Player_transition x ->
      let st = player_transition x trade_map in
      show_screen st trade_map
  | TradeResults x ->
      let st = show_trade_results x in
      show_screen st trade_map
  | AlteredRoster x ->
      let st = show_new_roster x trade_map in
      show_screen st trade_map

let main () =
  let _ = show_screen Welcome [] in
  ()

(* Execute the game engine. *)
let () = main ()
