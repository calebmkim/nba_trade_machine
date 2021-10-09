open Graphics 
open Data 
open Json_translation
open Consts

let _ = open_graph " 900x600"
let y = size_y ()

let get_size_horz s = s|>text_size |> fst 
let get_size_vert s = s|>text_size |> snd

(**Determines whether, when we are drawing text of size [size_vert], whether 
we should go down and write on the next line or move to the right *)
let get_new_point max_horz size_vert cur_x cur_y= 
  if (cur_y - size_vert) < 0 then (cur_x + max_horz+10, y - size_vert) else 
    ((cur_x),(cur_y - (size_vert)))

(*[get_max_size] is the horizontal size of the largest text in [lst] given
the current text settings. Requires: [lst] is a nonempty list of strings.*)
let get_max_size lst = lst|>List.sort 
(fun n1 n2 -> (get_size_horz n2 - get_size_horz n1))
|> List.hd |> get_size_horz

let print_name name cur_x cur_y max_horz=
  let size_horz = get_size_horz name in 
  let size_vert = get_size_vert name in 
  let new_point = get_new_point max_horz size_vert cur_x cur_y in 
  let () = moveto (fst new_point) (snd new_point) in 
  let () = draw_string (name) in
  let () = draw_rect (fst new_point) (snd new_point) size_horz size_vert in 
  let () = moveto (fst new_point) (current_y ())
  in (build_button name new_point size_horz size_vert)

let handle_click_roster st settings = Teams (get_list_transition settings) 

let show_team_roster settings= 
  open_graph_our_settings "";
  moveto 0 y; 
  let roster = get_roster_names_by_name (get_name_setting settings) in 
  let max_horz = get_max_size (roster) in 
  let button_list = List.map (fun x -> print_name x (current_x ()) (current_y ()) max_horz) roster in 
  (ignore button_list);
  let st = wait_next_event [Button_down] in 
  handle_click_roster st (settings)  

let handle_click_teams st lst cur_teams =
    try 
    let team = List.find (fun x -> button_clicked x (st.mouse_x) (st.mouse_y)) lst in 
    Team_transition (build_setting team.text cur_teams)
  with 
  _ -> Teams cur_teams 

let show_team_list team_list = 
  open_graph_our_settings "";
  moveto 0 y;
  let max_horz = get_max_size team_names in 
  let button_list = List.map (fun x -> print_name x (current_x ()) (current_y ()) max_horz)team_names in
  let () = set_color blue in 
  let _ = print_name "Teams Currently In Trade" (current_x ()) (current_y ()) max_horz in 
  let current_teams = List.map (fun x -> print_name x (current_x ()) (current_y ()) max_horz) team_list in 
  let () = set_color black in 
  let st = wait_next_event [Button_down] in 
  handle_click_teams st button_list (List.map (get_button_text) current_teams)

