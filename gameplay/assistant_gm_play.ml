open Data
open Json_translation
open Graphics
open State
open Button
open Assistant_gm_math
open Trademap

exception TooManyAttributes

let all_attributes =
  [
    "3 Point Shooting";
    "Defense";
    "Rebounding";
    "Playmaking";
    "Scoring";
    "Athleticism";
  ]

let handle_click_teams_gm st team_list back_button =
  if is_button_clicked back_button st then (GMTeams, true)
  else
    try
      let tm = find_clicked_button st team_list in
      (GMAttributes tm, false)
    with
    | NoButtonClicked -> (GMTeams, false)

let show_teams_gm () =
  Common_functions.start_state (size_y ());
  let msg = "Pick a team to manage" in
  let teams = Json_translation.team_names in
  let max_horz = get_max_size (msg :: teams) in
  let () = set_color blue in
  let _ = make_button msg in
  let () = set_color black in
  let teams_list = make_button_list ~max_horz teams in
  let () = set_color red in
  let back_button = make_button "Go Back to Home Screen" in
  let () = set_color black in
  let st = wait_next_event [ Button_down ] in
  handle_click_teams_gm st teams_list back_button

(*For debuggin purposes*)
let print_hd_if_exists lst =
  match lst with
  | [] -> ()
  | h :: t -> print_endline h

(**[list_empty lst] is true if [lst] is empty and false otherwise*)
let list_empty = function
  | [] -> true
  | h :: t -> false

let list_longer_than_3 lst = List.length lst > 3

(**[final_button_clicked att tm] handles the situation if the
   final_button is clicked. Makes sure too many nor too few teams are in
   the trade, before moving on.*)
let final_button_clicked cur_attributes team =
  if list_empty cur_attributes then
    ( GMError
        ( "You need to identify at least one team need",
          GMAttributes team ),
      cur_attributes )
  else if list_longer_than_3 cur_attributes then
    ( GMError
        ( "You can only identify at most three team needs",
          GMAttributes team ),
      cur_attributes )
  else (GMRoster team, cur_attributes)

let handle_click_attributes
    st
    att_buttons
    cur_att_buttons
    final_button
    cur_attributes
    team
    back_button =
  try
    if is_button_clicked back_button st then (GMTeams, [])
    else
      let added_attribute = find_clicked_button st att_buttons in
      (GMAttributes team, added_attribute :: cur_attributes)
  with
  | NoButtonClicked -> (
      try
        let removed_attribute =
          find_clicked_button st cur_att_buttons
        in
        ( GMAttributes team,
          List.filter (fun x -> x <> removed_attribute) cur_attributes
        )
      with
      | NoButtonClicked ->
          if is_button_clicked final_button st then
            final_button_clicked cur_attributes team
          else (GMAttributes team, cur_attributes))

let pick_attributes team cur_attributes =
  Common_functions.start_state (size_y ());
  let msg = "Please Select the " ^ team ^ "'s needs. Pick at most 3." in
  let current_attributes_msg =
    "Currently, here are the needs you identified"
  in
  let finalize_msg = "Move on" in
  let msgs = [ msg; current_attributes_msg; finalize_msg ] in
  let attributes =
    List.filter
      (fun x -> List.mem x cur_attributes = false)
      all_attributes
  in
  let max_horz = get_max_size (attributes @ cur_attributes @ msgs) in
  let () = set_color blue in
  let _ = make_button msg in
  let () = set_color black in
  let attribute_buttons = make_button_list ~max_horz attributes in
  let () = set_color blue in
  let _ = make_button current_attributes_msg in
  let () = set_color black in
  let cur_attribute_buttons =
    make_button_list ~max_horz cur_attributes
  in
  let () = set_color red in
  let final_button = make_button finalize_msg in
  let () = set_color black in
  let () = set_color red in
  let back_button = make_button "Go Back" in
  let () = set_color black in
  let st = wait_next_event [ Button_down ] in
  handle_click_attributes st attribute_buttons cur_attribute_buttons
    final_button cur_attributes team back_button

let handle_click_gm_team status player_list team_name back_button =
  try
    if is_button_clicked back_button status then GMAttributes team_name
    else
      let player_name = find_clicked_button status player_list in
      GMRecommendation player_name
  with
  | NoButtonClicked -> GMRoster team_name

let show_gm_team attributes team_name =
  Common_functions.start_state (size_y ());
  let msg = "Select the player that you want to trade" in
  let roster = get_roster_names_by_name team_name in
  let _ = make_button msg in
  let player_list = make_button_list roster in
  let () = set_color red in
  let back_button = make_button "Go Back" in
  let () = set_color black in
  let st = wait_next_event [ Button_down ] in
  handle_click_gm_team st player_list team_name back_button

let handle_click_trade_recommendations () = GMTeams

let show_trade_recommendation player attributes =
  Common_functions.start_state (size_y ());
  try
    let tmap = trade_player player attributes in
    let all_strings = get_all_strings tmap in
    let max_horz = get_max_size all_strings in
    let tm_assoc = to_assoc_trademap tmap in
    (*Sort so that the teams appear in the same order each time*)
    let _ = make_trademap_buttons tm_assoc max_horz in
    let _ = wait_next_event [ Button_down ] in
    handle_click_trade_recommendations ()
  with
  | NoMatch s -> GMError (s, GMRoster (get_team_of_player player))
