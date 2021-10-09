open Graphics 
open Gameplay 
open Consts 
open Team_list
open Transition
open WelcomeScreen

let rec show_screen cur_state = 
  match cur_state with 
  |Welcome -> let st = show_welcome Welcome in (show_screen st)
  |Roster x -> let st = (show_team_roster x) in (show_screen st)
  |Teams x -> let st = show_team_list x in  (show_screen st)
  |Team_transition x -> let st = team_options x in (show_screen st)

let main () =
  let _ = show_screen Welcome 
  in ()
  

(* Execute the game engine. *)
let () = main ()

