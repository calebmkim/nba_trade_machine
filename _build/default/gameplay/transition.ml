open Graphics
open Main_pages
open Data
open Json_translation
open Common_functions
open Button
open States

let handle_click_team_transition st lst name team_list : 'a * 'b =
  let setting = build_setting name team_list in
  try
    let decision = find_clicked_button st lst in
    if decision = "Examine Roster" then
      ( Roster (get_name_setting setting, false),
        make_trade_map team_list )
    else if List.mem name team_list then
      (Teams, make_trade_map team_list)
    else (Teams, make_trade_map (name :: team_list))
  with
  | _ -> (Team_transition name, make_trade_map team_list)

let team_transition team_name trade_map =
  start_state (size_y ());
  let _ = make_button_list [ "You have selected the " ^ team_name ] in
  let opts = [ "Add team to trade"; "Examine Roster" ] in
  let option_buttons = make_button_list opts in
  let st = wait_next_event [ Button_down ] in
  handle_click_team_transition st option_buttons team_name
    (teams_in_trade trade_map)

let get_possible_teams player trade_map =
  let team_of_player = get_team_of_player player in
  let team_list = List.map fst trade_map in
  List.filter (fun x -> x != team_of_player) team_list

let handle_click_player_transition st team_buttons name trade_map =
  try
    let destination = find_clicked_button st team_buttons in
    let old_incoming_players = List.assoc destination trade_map in
    let new_incoming_players =
      if List.mem name old_incoming_players then old_incoming_players
      else name :: old_incoming_players
    in
    let new_trade_map =
      (destination, new_incoming_players)
      :: List.remove_assoc destination trade_map
    in
    (FinalTeams, new_trade_map)
  with
  | _ -> (Player_transition name, trade_map)

let player_transition name trade_map =
  let max_horz =
    get_max_size
      (("Where would you like to trade " ^ name ^ " to?") :: team_names)
  in
  start_state (size_y ());
  let _ =
    make_button ~max_horz
      ("Where would you like to trade " ^ name ^ " to?")
  in
  let team_names = get_possible_teams name trade_map in
  let button_list = make_button_list ~max_horz team_names in
  let st = wait_next_event [ Button_down ] in
  handle_click_player_transition st button_list name trade_map
