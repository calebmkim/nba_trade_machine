open Graphics
open Main_pages
open Data
open Json_translation
open Common_functions
open Button
open States

let handle_click_team_transition st lst name team_list =
  let setting = build_setting name team_list in
  try
    let decision = find_clicked_button st lst in
    if decision = "Examine Roster" then Roster (setting, false)
    else if List.mem name team_list then Teams team_list
    else Teams (name :: team_list)
  with
  | _ -> Team_transition setting

let team_options setting =
  start_state (size_y ());
  let _ =
    let messages =
      [ "You have selected the " ^ get_name_setting setting ]
    in
    make_button_list messages
  in
  let opts = [ "Add team to trade"; "Examine Roster" ] in
  let option_list = make_button_list opts in
  let st = wait_next_event [ Button_down ] in
  handle_click_team_transition st option_list
    (get_name_setting setting)
    (get_list_setting setting)

let get_possible_teams name trade_map =
  let team = get_team_of_player name in
  let team_list = List.map fst trade_map in
  List.filter (fun x -> x != team) team_list

let handle_click_player_transition st button_list name trade_map =
  try
    let destination = find_clicked_button st button_list in
    let old_incoming_players = List.assoc destination trade_map in
    let new_incoming_players =
      if List.mem name old_incoming_players then old_incoming_players
      else name :: old_incoming_players
    in
    let new_trade_map =
      (destination, new_incoming_players)
      :: List.remove_assoc destination trade_map
    in
    FinalTeams new_trade_map
  with
  | _ -> Player_transition name

let player_transition name trade_map =
  start_state (size_y ());
  let _ =
    let messages =
      [ "Where would you like to trade " ^ name ^ " to?" ]
    in
    make_button_list messages
  in
  let team_names = get_possible_teams name trade_map in
  let button_list = make_button_list team_names in
  let st = wait_next_event [ Button_down ] in
  handle_click_player_transition st button_list name trade_map
