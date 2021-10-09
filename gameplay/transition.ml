open Consts
open Graphics 
open Team_list
open Data 
open Json_translation

let handle_click_team_transition st lst name team_list = let settings = 
(build_setting name team_list ) in
try let decision = List.find
 (fun a -> button_clicked a (st.mouse_x) (st.mouse_y)) lst in 
 if (get_button_text decision) = "Examine Roster" then Roster (settings,false) 
else if (List.mem name team_list) then Teams team_list else Teams (name::team_list)
with 
_ -> Team_transition (build_setting name team_list)

let team_options setting = 
  open_graph_our_settings "";
  let y = size_y () in  
  let vert_text_size = snd (text_size "") in 
  let () = moveto 0 (y - vert_text_size) in 
  let () = draw_string ("You have selected the " ^ (get_name_setting setting)) in 
  let () = moveto (0) (current_y () - vert_text_size) in 
  let () = draw_string ("What would you like to do next?") in 
  let () = moveto (0) (current_y ()) in 
  let opts = ["Add team to trade"; "Examine Roster"] in 
  let max_horz = get_max_size opts in 
  let option_list = List.map (fun x -> print_name x (current_x ()) (current_y ()) max_horz) opts in 
  let st = wait_next_event [Button_down] in 
  handle_click_team_transition st option_list (get_name_setting setting)
   (get_list_transition setting) 
  
let get_possible_teams name trade_map = 
  let team = get_team_of_player name in 
  let team_list = List.map (fst) trade_map in 
  List.filter (fun x -> x != team) team_list 

let handle_click_player_transition st button_list name trade_map= try 
let destination = (List.find (fun b -> button_clicked b (st.mouse_x) (st.mouse_y)) button_list)|>get_button_text in 
let old_incoming_players = List.assoc (destination) trade_map in 
let new_incoming_players = if (List.mem name old_incoming_players) then old_incoming_players 
else (name::old_incoming_players) in 
let new_trade_map = (destination, new_incoming_players) :: (List.remove_assoc (destination) trade_map) in 
FinalTeams new_trade_map
with 
_ -> Player_transition name 


let player_transition name trade_map = 
  open_graph_our_settings ""; 
  let vert_text_size = snd (text_size "") in 
  let () = moveto 0 (y - vert_text_size) in 
  let () = draw_string ("Where would you like to trade " ^ name ^ " to?") in 
let () = moveto (0) (current_y ())  in 
let team_names = get_possible_teams name trade_map in 
let max_horz = get_max_size team_names in 
let button_list = List.map (fun x -> print_name x (current_x ()) (current_y ()) max_horz) team_names in 
let st = wait_next_event [Button_down] in 
handle_click_player_transition st button_list name trade_map 






