open Consts
open Graphics 
open Team_list

let handle_click_team_transition st lst name team_list = try let option = List.find
 (fun a -> is_clicked (st.mouse_x) (st.mouse_y) a) lst in 
 if option.text = "Examine Roster" then Roster {team = name; cur_teams = team_list} 
else if (List.mem name team_list) then Teams team_list else  Teams (name::team_list)
with 
_ -> Team_transition {team = name; cur_teams = team_list} 
let print_option name = 
  let size_horz = get_size_horz name in 
  let size_vert = get_size_vert name in 
  let og_x = current_x () in 
  let og_y = current_y () in 
  let () = draw_string name in 
  let () = draw_rect (og_x) (og_y) size_horz size_vert in 
  let () = moveto (og_x) (og_y - size_vert) in 
  {text= name; ll = (og_x, og_y); ur = (og_x + size_horz, og_y + size_vert) }

let team_options setting = 
  let _ = open_graph " 900x600" in 
  let () = set_font "-*-lucidatypewriter-*-*-*-*-*-*-*-*-*-150-*-*" in
  let y = size_y () in  
  let vert_text_size = snd (text_size "") in 
  let () = moveto 0 (y - vert_text_size) in 
  let () = draw_string ("You have selected the " ^ (get_name_transition setting)) in 
  let () = moveto (0) (current_y () - vert_text_size) in 
  let () = draw_string ("What would you like to do next?") in 
  let () = moveto (0) (current_y () - vert_text_size) in 
  let option_list =  List.map (print_option) ["Add team to trade"; "Examine Roster"] in 
  let st = wait_next_event [Button_down] in 
  handle_click_team_transition st option_list (get_name_transition setting)
   (get_list_transition setting) 
  
  

