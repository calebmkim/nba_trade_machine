open Consts
open Graphics 
open Team_list

let handle_click_team_transition st lst name team_list = let settings = 
(build_setting name team_list ) in
try let decision = List.find
 (fun a -> button_clicked a (st.mouse_x) (st.mouse_y)) lst in 
 if decision.text = "Examine Roster" then Roster settings
else if (List.mem name team_list) then Teams team_list else  Teams (name::team_list)
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
  
  

