open Graphics
open MainPages
open Data
open JsonTranslation
open Button
open Trademap

(**[handle_click_team_transition st options name team_list] is the
   ([st, tm]) tuple in which [st] is the current [st] given the user's
   click. [tm] is the new [trade_map] for the trade: if the user clicks
   the add team to trade button, then update trademap accordingly. If
   the user does anything else, no updates to the trademap are needed. *)
let handle_click_team_transition st options team_name trade_map =
  try
    let decision = find_clicked_button st options in
    if decision = "Examine Roster" then
      (Roster (team_name, false), trade_map)
    else if decision = "Remove team from trade" then
      (Teams, remove_team_from_trade team_name trade_map)
    else (Teams, add_team_to_trade team_name trade_map)
  with
  | NoButtonClicked -> (Team_transition team_name, trade_map)

let team_transition team_name trade_map =
  start_state (size_y ());
  let _ = make_button_list [ "You have selected the " ^ team_name ] in
  let action =
    if is_team_in_trade team_name trade_map then
      "Remove team from trade"
    else "Add team to trade"
  in
  let opts = [ action; "Examine Roster" ] in
  let option_buttons = make_button_list opts in
  let st = wait_next_event [ Button_down ] in
  handle_click_team_transition st option_buttons team_name trade_map

let print_tm tm = tm |> get_all_strings |> List.map print_endline

(**[handle_click_team_transition st team_buttons name trade_map] is the
   ([st, tm]) tuple in which [st] is the current [st] given the user's
   click. [tm] is the new [trade_map] for the trade: if the user clicks
   one of the possible destination buttons in [team_buttons], then
   update trademap accordingly. If the user does anything else, no
   updates to the trademap are needed. *)
let handle_click_player_transition st team_buttons name trade_map =
  try
    let destination = find_clicked_button st team_buttons in
    let new_trade_map =
      add_player_to_trade name destination trade_map
    in
    (FinalTeams, new_trade_map)
  with
  | NoButtonClicked -> (Player_transition name, trade_map)

let player_transition name trade_map =
  let msg = "Where would you like to trade " ^ name ^ " to?" in
  let max_horz = get_max_size (msg :: team_names) in
  start_state (size_y ());
  let _ = make_button ~max_horz msg in
  let team_names = get_valid_destinations name trade_map in
  let button_list = make_button_list ~max_horz team_names in
  let st = wait_next_event [ Button_down ] in
  handle_click_player_transition st button_list name trade_map
