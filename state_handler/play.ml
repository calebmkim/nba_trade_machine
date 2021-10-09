open Graphics 
open Gameplay 
open Consts 
open Team_list
open Transition
open WelcomeScreen

let rec show_screen cur_state trade_map= 
  match cur_state with 
  |Welcome -> let st = show_welcome Welcome in (show_screen st trade_map)
  |Roster x -> let st = (show_team_roster (fst x) (snd x)) in (show_screen st trade_map)
  |Teams x -> let st = show_team_list x in  (show_screen st trade_map) 
  |Team_transition x -> let st = team_options x in (show_screen st trade_map)
  |FinalTeams x -> let st = show_final_teams x in (show_screen st trade_map) 
  |Player x -> let st = show_player (fst x) (snd x) trade_map in (show_screen st trade_map)
  |Player_transition x -> let st = player_transition x trade_map in (show_screen st trade_map)

let main () =
  let _ = show_screen Welcome []
  in ()
  

(* Execute the game engine. *)
let () = main ()

