open Data
open JsonTranslation
open Trademap
open HandleTeamStats
open HandleSalaryData

let min_per_game = 48.

let games_per_season = 82.

let players_on_court = 5.

let unbox_stat = function
  | None -> 0.
  | Some x -> x

(**[avg_player team] is the expected win shares per 48 minutes of an
   average player on [team], given [team]'s win percentage last year. *)
let avg_player team =
  team |> get_win_percentage |> fun win_pct ->
  win_pct /. players_on_court

(**[ws_per_48 player] is win shares per 48 minutes of [player].
   Evaluates to 0. if the stat is not available. Requires: [player] is
   the valid name of an NBA player. *)
let ws_per_48 player_name =
  let total_ws =
    unbox_stat (ows player_name) +. unbox_stat (dws player_name)
  in
  let minutes_played = unbox_stat (minutes_played player_name) in
  let ws_per_min =
    if minutes_played = 0. then 0. else total_ws /. minutes_played
  in
  ws_per_min *. min_per_game

(**[num_player_change team tmap] is the net amount of players leaving
   [team] if the trade were to go through. For example if [tmap] were
   such that [team] would trade 2 players and recieve 1, then
   [num_player_change team tmap] would evaluate to 1. Requires: [team]
   is a valid NBA team represented in [trade_map]*)
let num_player_change team trade_map =
  let incoming = players_acquiring team trade_map |> List.length in
  let outgoing = players_losing team trade_map |> List.length in
  outgoing - incoming

(**[win_contributions plist] is the combined amount of win shares per 48
   minutes for each player in [plist]*)
let win_contributions player_list =
  List.fold_left (fun acc elt -> acc +. ws_per_48 elt) 0. player_list

(**[wins_departing team tmap] is the combined win shares per 48 minutes
   of all the players leaving [team] if the trade represented by
   [trade_map] were to go through. Requires: [team] is a valid NBA team
   represented by [trade_map]*)
let wins_departing team trade_map =
  let outgoing_players = players_losing team trade_map in
  win_contributions outgoing_players

(**[wins_gaining team tmap] is the combined win shares per 48 minutes of
   all the players acquired by [team] if the trade represented by
   [trade_map] were to go through. Requires: [team] is a valid NBA team
   represented by [trade_map]*)
let wins_gaining team trade_map =
  let incoming_plyaers = players_acquiring team trade_map in
  win_contributions incoming_plyaers

let win_differential team trade_map =
  (*let _ = print_endline "Calculating" in*)
  let correction =
    (num_player_change team trade_map |> float_of_int)
    *. avg_player team
  in
  wins_gaining team trade_map
  -. wins_departing team trade_map
  +. correction
  |> ( *. ) games_per_season

(**[get_total_salaries player_list] is the total salaries of players in
   [player_list]. Requires: each string in [player_list] is a current
   NBA player's name *)
let get_total_salaries player_list =
  List.fold_left (fun acc pl -> salary pl + acc) 0 player_list

(**[get_incoming_salaries team tmap] is the total incoming salaries that
   [team] will receive given [tmap] goes through. Requires [team] is a
   team involved in trade [tmap]*)
let get_incoming_salaries team trade_map =
  let incoming_players = players_acquiring team trade_map in
  get_total_salaries incoming_players

(**[get_outgoing_salaries] is the total outgoing salaries that [team]
   will give up given [tmap] goes through. Requires [team] is a team
   involved in trade [tmap]*)
let get_outgoing_salaries team trade_map =
  let outgoing_players = players_losing team trade_map in
  get_total_salaries outgoing_players

let is_trade_viable team trade_map =
  let incoming = get_incoming_salaries team trade_map in
  let outgoing = get_outgoing_salaries team trade_map in
  (* for debugging purposes: let _ = print_endline team in let _ =
     print_endline (string_of_int incoming) in let _ = print_endline
     (string_of_int outgoing) in*)
  let net_change = outgoing - incoming in
  let team_difference = get_cap_differential team in
  let new_difference = team_difference + net_change in
  if new_difference > 0 then true
  else
    let incoming_threshold =
      (float_of_int outgoing *. 1.25) +. 100000.
    in
    float_of_int incoming <= incoming_threshold

(** [get_new_roster team trade_map] is the list of players that will be
    on [team] after [trade_map] executes. *)
let get_new_roster team trade_map =
  let outgoing_players = players_losing team trade_map in
  let incoming_players = players_acquiring team trade_map in
  let cur_players = get_roster_names_by_name team in
  let removed_outgoing =
    List.filter
      (fun player -> (List.mem player) outgoing_players = false)
      cur_players
  in
  removed_outgoing @ incoming_players

let per_slope : float option ref = ref None

let () =
  let ic = open_in "per_slope.dat" in
  try
    let line = input_line ic in
    close_in ic;
    (* read line, discard \n *)
    per_slope := Some (float_of_string line)
    (* close the input channel *)
  with
  | e -> failwith "Couldn't get per_slope"
(* exit with error: files are closed but channels are not flushed *)

let get_per_slope () =
  match !per_slope with
  | None ->
      failwith "The model isn't built yet. Try running make per_build"
  | Some x -> x

let win_diff_per team trade_map =
  let og_roster = get_roster_names_by_name team in
  let new_roster = get_new_roster team trade_map in
  let og_per_avg = JsonTranslation.get_per_ratio og_roster in
  let new_per_avg = JsonTranslation.get_per_ratio new_roster in
  (new_per_avg -. og_per_avg)
  *. (get_per_slope () /. 100.)
  *. games_per_season
