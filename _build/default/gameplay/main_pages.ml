open Graphics
open Data
open Json_translation
open Common_functions
open Button
open Trademap
open State
open Trade_math

(* type wrongclick = { | message of string; | prev of state; }

   exception WrongClick of wrongclick *)
(* let ex f = print_endline "Incorrect click. Taking you back to the
   main page"; f exit 0 at_exit f *)

let ex () =
  print_endline "Incorrect click. Taking you back to the main page";
  exit 0

let handle_click_roster st player_list are_teams_picked =
  try
    let player_name = find_clicked_button st player_list in
    Player (player_name, are_teams_picked)
  with
  | _ -> if are_teams_picked then FinalTeams else Teams

let show_roster team_name are_teams_picked =
  start_state (size_y ());
  let roster = get_roster_names_by_name team_name in
  let player_list = make_button_list roster in
  let st = wait_next_event [ Button_down ] in
  handle_click_roster st player_list are_teams_picked

let handle_click_teams st all_teams_list teams_in_trade final_button =
  try
    if is_button_clicked final_button st then
      if List.length teams_in_trade >= 2 then FinalTeams
      else
        Error ("At least 2 teams must be involved in the trade", Teams)
    else
      let all_team_buttons = all_teams_list @ teams_in_trade in
      let team_name = find_clicked_button st all_team_buttons in
      Team_transition team_name
  with
  | NoButtonClicked -> Teams

let show_team_list teams_in_trade =
  start_state (size_y ());
  let teams = Json_translation.team_names in
  let string_lst =
    "Finalize Teams" :: "Teams Currently In Trade" :: teams
  in
  let max_horz = get_max_size string_lst in
  let all_teams_list = make_button_list ~max_horz teams in
  let () = set_color blue in
  let _ = make_button ~max_horz "Teams Currently In Trade" in
  let teams_in_trade = make_button_list ~max_horz teams_in_trade in
  let () = set_color red in
  let finalize_button = make_button ~max_horz "Finalize Teams" in
  let () = set_color black in
  let st = wait_next_event [ Button_down ] in
  handle_click_teams st all_teams_list teams_in_trade finalize_button

let handle_click_final_teams st team_list trade_map finish_button =
  try
    if is_button_clicked finish_button st then
      if valid_trade trade_map then TradeResults
      else
        Error
          ("Each team must be receiving at least 1 player", FinalTeams)
    else
      let team = find_clicked_button st team_list in
      Roster (team, true)
  with
  | NoButtonClicked -> FinalTeams

let show_final_teams trade_map =
  start_state (size_y ());
  let all_strings = get_all_strings trade_map in
  let max_horz = get_max_size all_strings in
  let tm_assoc = to_assoc_trademap trade_map in
  (*Sort so that the teams appear in the same order each time*)
  let trademap_team_buttons = make_trademap_buttons tm_assoc max_horz in
  set_color red;
  let trade_button = make_button ~max_horz "Execute Trade" in
  set_color black;
  let st = wait_next_event [ Button_down ] in
  handle_click_final_teams st trademap_team_buttons trade_map
    trade_button

(** [show_player_stat] returns the player statistics let
    show_player_stat player_stat = start_state (size_y ()); let _ =
    set_window_title "Player Statistics" in let button_list =
    make_button_list player_stat in let () = set_color black in let _ =
    make_button "Statistics of Player" (current_x ()) (current_y ())
    max_horz*)

let handle_player_click st player_name =
  let team_name = get_team_of_player player_name in
  Roster (team_name, false)

let string_of_stat = function
  | None -> "N/A"
  | Some x -> string_of_float x

let list_of_stats player_name =
  let o_win_shares =
    "Offensive Win Shares: " ^ string_of_stat (ows player_name)
  in
  let d_win_shares =
    "Defensive Win Shares: " ^ string_of_stat (dws player_name)
  in
  [ o_win_shares; d_win_shares ]

let show_player player_name =
  start_state (size_y ());
  let name_stats_list = player_name :: list_of_stats player_name in
  let _ = make_button_list name_stats_list in
  let st = wait_next_event [ Button_down ] in
  handle_player_click st player_name

let handle_player_trade_click st name trade_button trade_map =
  if is_button_clicked trade_button st then
    let command = get_button_text trade_button in
    if command = "Trade this Player" then
      (Player_transition name, trade_map)
    else (FinalTeams, remove_player_from_trade name trade_map)
  else (FinalTeams, trade_map)

let show_player_trade player_name trade_map =
  start_state (size_y ());
  let remove_msg = "Remove Player from Trade" in
  let add_msg = "Trade this Player" in
  let player_stats = list_of_stats player_name in
  let max_horz =
    get_max_size ([ player_name; add_msg; remove_msg ] @ player_stats)
  in
  let _ = make_button ~max_horz player_name in
  let _ = make_button_list ~max_horz player_stats in
  let () = set_color red in
  let trade_msg =
    if is_player_in_trade player_name trade_map then remove_msg
    else add_msg
  in
  let trade_button = make_button ~max_horz trade_msg in
  let () = set_color black in
  let st = wait_next_event [ Button_down ] in
  handle_player_trade_click st player_name trade_button trade_map

let handle_trade_results_click st team_buttons altered_rosters trade_map
    =
  try
    let team_selected = find_clicked_button st team_buttons in
    let fst_of_three (x, y, z) = x in
    let team =
      List.find
        (fun x -> fst_of_three x = team_selected)
        altered_rosters
    in
    AlteredRoster team
  with
  | NoButtonClicked -> FinalTeams
  | Not_found -> FinalTeams

let win_difference_list trade_map =
  List.map
    (fun team ->
      team ^ ": "
      ^ (win_differential team trade_map |> string_of_float)
      ^ " Wins")
    (trade_map |> teams_in_trade)

let show_trade_results trade_map =
  start_state (size_y ());
  let team_names = teams_in_trade trade_map in
  let team_buttons = make_button_list team_names in
  let _ = make_button_list (win_difference_list trade_map) in
  let altered_rosters = change_rosters trade_map in
  let st = wait_next_event [ Button_down ] in
  handle_trade_results_click st team_buttons altered_rosters trade_map

let show_altered_roster (name, old, incoming) =
  start_state (size_y ());
  let n = [ name ] in
  set_color blue;
  let _ = make_button_list n in
  set_color black;
  let _ = make_button_list old in
  set_color red;
  let _ = make_button_list incoming in
  set_color black;
  let _ = wait_next_event [ Button_down ] in
  TradeResults

let show_error_message msg =
  start_state (size_y ());
  let _ = make_button msg in
  let _ = wait_next_event [ Button_down ] in
  ()
