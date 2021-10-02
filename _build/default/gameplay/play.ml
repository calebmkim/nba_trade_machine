open Graphics 
open Data 
open Json_translation


let _ = open_graph "";;
let show_welcome = 
  draw_string "Welcome to our NBA Trade Machine. Here are the instructions: ";
wait_next_event [Button_down]

let _ = clear_graph

let _ = open_graph ""

let y = size_y ()

let show_team_list = 
  moveto 0 (y-10);
  List.iter (fun x -> draw_string x; moveto 0 5) team_names;
  wait_next_event [Button_down]


(*(
let play_game = open_graph ""; print_endline "hi"

let main () =
  play_game



(* Execute the game engine. *)
let () = main ()
*)
