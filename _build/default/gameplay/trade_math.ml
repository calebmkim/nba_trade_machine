open Data
open Json_translation
open Trademap
open Handle_team_stats

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

let get_total_salary player_list = ()
