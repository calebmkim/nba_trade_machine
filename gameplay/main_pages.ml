open Graphics
open Data
open Json_translation
open Consts
open Common_functions

let handle_click_roster
    st
    settings
    player_list
    are_teams_picked
    trade_map =
  try
    let player_name = find_clicked_button st player_list in
    let teams = get_list_setting settings in
    Player (build_setting player_name teams, are_teams_picked)
  with
  | _ ->
      if are_teams_picked then FinalTeams trade_map
      else Teams (get_list_setting settings)

let show_team_roster settings are_teams_picked trade_map =
  start_state (size_y ());
  let roster = get_roster_names_by_name (get_name_setting settings) in
  let button_list = make_button_list roster in
  let st = wait_next_event [ Button_down ] in
  handle_click_roster st settings button_list are_teams_picked trade_map

let handle_click_teams st button_list cur_teams final_button =
  try
    let x = st.mouse_x in
    let y = st.mouse_y in
    if button_clicked final_button x y && List.length cur_teams >= 2
    then FinalTeams (List.map (fun x -> (x, [])) cur_teams)
    else
      let team_name = find_clicked_button st button_list in
      Team_transition (build_setting team_name cur_teams)
  with
  | _ -> Teams cur_teams

let show_team_list team_list =
  start_state (size_y ());
  let max_horz = get_max_size team_names in
  let button_list = make_button_list team_names in
  let () = set_color blue in
  let _ =
    print_name "Teams Currently In Trade" (current_x ()) (current_y ())
      max_horz
  in
  let current_teams =
    List.map
      (fun x -> print_name x (current_x ()) (current_y ()) max_horz)
      team_list
  in
  let () = set_color red in
  let finalize_button =
    print_name "Finalize Teams" (current_x ()) (current_y ()) max_horz
  in
  let () = set_color black in
  let st = wait_next_event [ Button_down ] in
  handle_click_teams st button_list
    (List.map get_button_text current_teams)
    finalize_button

let handle_click_final_teams st team_list trade_map =
  let team_strings = List.map (fun x -> get_button_text x) team_list in
  try
    let team = find_clicked_button st team_list in
    (Roster (build_setting team team_strings, true), trade_map)
  with
  | _ -> (FinalTeams trade_map, trade_map)

let get_max_size_overall trade_map =
  let combined_list =
    List.flatten (List.map (fun (x, y) -> x :: y) trade_map)
  in
  get_max_size combined_list

let print_trademap_pair max_size_overall (team, players_recieving) =
  set_color blue;
  let team_button =
    print_name team (current_x ()) (current_y ()) max_size_overall
  in
  let _ =
    print_name "will recieve: " (current_x ()) (current_y ())
      max_size_overall
  in
  set_color black;
  let _ =
    List.map
      (fun x ->
        print_name x (current_x ()) (current_y ()) max_size_overall)
      players_recieving
  in
  team_button

let comapre_team_tuples x y = if fst x < fst y then 1 else -1

let show_final_teams trade_map =
  start_state (size_y ());
  let max_horz = get_max_size_overall trade_map in
  let trade_map = List.sort comapre_team_tuples trade_map in
  let button_list = List.map (print_trademap_pair max_horz) trade_map in
  let st = wait_next_event [ Button_down ] in
  handle_click_final_teams st button_list trade_map

let print_player_name setting =
  let name = get_name_setting setting in
  let max_horz = get_size_horz name in
  print_name name (current_x ()) (current_y ()) max_horz

let handle_player_click st setting =
  let player_name = get_name_setting setting in
  let team_name = get_team_of_player player_name in
  let new_setting = { setting with identifier = team_name } in
  Roster (new_setting, false)

let show_player setting =
  start_state (size_y ());
  let _ = print_player_name setting in
  let st = wait_next_event [ Button_down ] in
  handle_player_click st setting

let handle_player_trade_click st name trade_button trade_map =
  if button_clicked trade_button st.mouse_x st.mouse_y then
    Player_transition name
  else FinalTeams trade_map

let show_player_trade setting trade_map =
  start_state (size_y ());
  let name = get_name_setting setting in
  let max_horz = get_size_horz name in
  let _ = print_name name (current_x ()) (current_y ()) max_horz in
  let () = set_color red in
  let trade_button =
    print_name "Trade This Player" (current_x ()) (current_y ())
      max_horz
  in
  let () = set_color black in
  let st = wait_next_event [ Button_down ] in
  handle_player_trade_click st name trade_button trade_map