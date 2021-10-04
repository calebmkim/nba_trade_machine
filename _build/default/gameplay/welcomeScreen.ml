open Graphics 

let _ = open_graph " 900x600";;
let () = set_font "-*-lucidatypewriter-*-*-*-*-*-*-*-*-*-150-*-*"

type state = Welcome | Teams | Roster of string 

let handle_click st = 
  Teams 

let show_welcome = 
  draw_string "Welcome to our NBA Trade Machine. Click to Start";
let st = wait_next_event [Button_down] in 
  (ignore st)

