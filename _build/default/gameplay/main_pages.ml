open Graphics
open Data
open Json_translation
open Button
open Trademap
open Trade_math

type state =
  | Welcome
  | Teams
  | Roster of (string * bool)
  | Team_transition of string
  | Player of (string * bool)
  | Player_transition of string
  | FinalTeams
  | TradeResults
  | AlteredRoster of (string * string list * string list)
  | Error of (string * state)

type gmstate =
  | GMTeams
  | GMRoster of string
  | GMAttributes of string
  | GMRecommendation of string
  | GMError of (string * gmstate)

(**[open_graph_our_settings] opens a 900x600 graph.*)
let open_graph_our_settings s =
  let _ = open_graph " 900x600" in
  ()
(*set_font "-*-*-*-*-*-*-*-*-*-*-*-150-*-*"*)

let start_state max_y =
  open_graph_our_settings "";
  moveto 0 max_y

let handle_click_welcome st tm_button gm_button =
  if is_button_clicked tm_button st then "Teams"
  else if is_button_clicked gm_button st then "GM"
  else "Welcome"

let show_welcome t =
  let _ = start_state (size_y ()) in
  let _ =
    [ "Welcome to our NBA Trade Machine"; "What would you like to do?" ]
    |> make_button_list
  in
  let tm_button = make_button "Play the Trade Machine" in
  let gm_button = make_button "Play as a GM for an NBA team" in
  let st = wait_next_event [ Button_down ] in
  handle_click_welcome st tm_button gm_button

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

let handle_final_button_click teams_in_trade =
  let num_teams = List.length teams_in_trade in
  if num_teams > 4 then
    ( Error
        ( "You can select at most 4 teams to be involved in the trade",
          Teams ),
      false )
  else if num_teams <= 1 then
    ( Error ("At least 2 teams must be involved in the trade", Teams),
      false )
  else (FinalTeams, false)

let handle_click_teams
    st
    all_teams_list
    teams_in_trade
    final_button
    back_button =
  if is_button_clicked back_button st then (Welcome, true)
  else
    try
      if is_button_clicked final_button st then
        handle_final_button_click teams_in_trade
      else
        let all_team_buttons = all_teams_list @ teams_in_trade in
        let team_name = find_clicked_button st all_team_buttons in
        (Team_transition team_name, false)
    with
    | NoButtonClicked -> (Teams, false)

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
  let back_button = make_button ~max_horz "Go Back to Home" in
  let () = set_color black in
  let st = wait_next_event [ Button_down ] in
  handle_click_teams st all_teams_list teams_in_trade finalize_button
    back_button

let handle_click_final_teams
    st
    tmap_buttons
    trade_map
    finish_button
    return_button =
  let team_list, incoming_players_list = List.split tmap_buttons in
  let players_list = List.flatten incoming_players_list in
  try
    if is_button_clicked return_button st then (Teams, true)
    else if is_button_clicked finish_button st then
      if valid_trade trade_map then (TradeResults, false)
      else
        ( Error
            ("Each team must be receiving at least 1 player", FinalTeams),
          false )
    else
      let team = find_clicked_button st team_list in
      (Roster (team, true), false)
  with
  | NoButtonClicked -> (
      try
        let player_name = find_clicked_button st players_list in
        (Player (player_name, true), false)
      with
      | NoButtonClicked -> (FinalTeams, false))

let show_final_teams trade_map =
  start_state (size_y ());
  let all_strings = get_all_strings trade_map in
  let max_horz = get_max_size all_strings in
  let tm_assoc = to_assoc_trademap trade_map in
  (*Sort so that the teams appear in the same order each time*)
  let trademap_team_buttons = make_trademap_buttons tm_assoc max_horz in
  set_color red;
  let trade_button = make_button ~max_horz "Execute Trade" in
  set_color blue;
  let return_button = make_button ~max_horz "Go Back" in
  set_color black;
  let st = wait_next_event [ Button_down ] in
  handle_click_final_teams st trademap_team_buttons trade_map
    trade_button return_button

let handle_player_click st player_name =
  let team_name = get_team_of_player player_name in
  Roster (team_name, false)

let string_of_stat = function
  | None -> "N/A"
  | Some x -> string_of_float x

let stringify_win_shares player_name =
  "Win Shares Per 48 Minutes: "
  ^
  if ws_per_48 player_name = 0. then "N/A"
  else string_of_float (ws_per_48 player_name)

let stringify_player_stat stat_name f player =
  stat_name ^ ": " ^ string_of_stat (f player)

let per_string = stringify_player_stat "PER" per

let pts_per_48_string = stringify_player_stat "Points/48" pts_per_48

let ast_pct_string = stringify_player_stat "Assist Percent" ast_pct

let reb_pct_string = stringify_player_stat "Rebound Percent" reb_pct

let def_rating_string = stringify_player_stat "Defensive Rating" drtg

let three_pt_pct_string =
  stringify_player_stat "Three Point Percent" three_pt_pct

let list_of_stats player_name =
  let ws_str = stringify_win_shares player_name in
  let per_str = per_string player_name in
  let pp48_str = pts_per_48_string player_name in
  let ast_pct_str = ast_pct_string player_name in
  let reb_pct_str = reb_pct_string player_name in
  let def_rating_str = def_rating_string player_name in
  let thr_pt_str = three_pt_pct_string player_name in
  [
    ws_str;
    per_str;
    pp48_str;
    ast_pct_str;
    reb_pct_str;
    def_rating_str;
    thr_pt_str;
  ]

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
      team ^ ": " ^ " Using Win Shares: "
      ^ (win_differential team trade_map |> string_of_float)
      ^ " Wins | Using PER: "
      ^ (win_diff_per team trade_map |> string_of_float)
      ^ " Wins")
    (trade_map |> teams_in_trade)

let trade_viable_message trade_map =
  let tms = teams_in_trade trade_map in
  let works_or_not =
    if List.for_all (fun tm -> is_trade_viable tm trade_map) tms then
      "This Trade, by Salary Cap Rules, would probably work out"
    else "This Trade, by Salary Cap Rules, would probably not work out"
  in
  let disclaimer =
    "However, this trade machine does not implement every single \
     exception and rule that the offical NBA salary cap rules dictate."
  in
  [ works_or_not; disclaimer ]

let show_trade_results trade_map =
  start_state (size_y ());
  let team_names = teams_in_trade trade_map in
  let team_buttons = make_button_list team_names in
  let _ = make_button_list (win_difference_list trade_map) in
  let _ = make_button_list (trade_viable_message trade_map) in
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
