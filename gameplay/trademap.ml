open Data
open JsonTranslation
open Graphics

type trade_map = (string * string list) list

let to_assoc_trademap m = m

let teams_in_trade tm = List.map fst tm

let make_trade_map teams = List.map (fun x -> (x, [])) teams

let is_valid_destination destination trade_map =
  let tms = List.map fst trade_map in
  List.filter (fun x -> x = destination) tms |> List.length |> ( = ) 1

let get_valid_destinations player trade_map =
  let team_of_player = get_team_of_player player in
  let team_list = List.map fst trade_map in
  List.filter (fun x -> x <> team_of_player) team_list

let add_player_to_trade player destination trade_map =
  if is_valid_destination destination trade_map then
    List.map
      (fun (team, players_recieving) ->
        if team = destination && not (List.mem player players_recieving)
        then (team, player :: players_recieving)
        else (team, players_recieving))
      trade_map
  else failwith "not a valid trade map and destination"

let valid_trade trade_map =
  let teams_receiving_nobody =
    trade_map |> List.map snd
    |> List.filter (fun x -> List.length x = 0)
  in
  List.length teams_receiving_nobody = 0

let get_all_strings trade_map =
  List.map (fun (x, y) -> x :: y) trade_map |> List.flatten

let sort_team_tuples =
  List.sort_uniq (fun (x, y) (x1, y1) -> Stdlib.compare x x1)

let remove_traded_player roster_list name =
  let remove_player name roster =
    List.filter (fun x -> x <> name) roster
  in
  List.map
    (fun (team_name, roster) -> (team_name, remove_player name roster))
    roster_list

let change_rosters trade_map =
  let original_rosters =
    List.map
      (fun (team, players) -> (team, get_roster_names_by_name team))
      trade_map
  in
  let traded_players = trade_map |> List.map snd |> List.flatten in
  let removed_players_roster =
    List.fold_left remove_traded_player original_rosters traded_players
    |> sort_team_tuples
  in
  (*sort so that [tm] and [removed_players_roster] are in the same
    order*)
  let tm = trade_map |> sort_team_tuples in
  List.map2
    (fun (team, old_players) (team', new_players) ->
      if team <> team' then
        failwith "Teams do not match when creating new rosters"
      else (team, old_players, new_players))
    removed_players_roster tm

let players_losing team_name trade_map =
  let departing_players =
    List.map
      (fun (team, players_recieving) ->
        List.filter
          (fun player -> get_team_of_player player = team_name)
          players_recieving)
      trade_map
  in
  List.flatten departing_players

let players_acquiring team_name trade_map =
  try
    List.find
      (fun (team, players_recieving) -> team_name = team)
      trade_map
    |> snd
  with
  | _ -> failwith "Team not in trade"

let is_team_in_trade team_name trade_map =
  List.exists (fun x -> x = team_name) (trade_map |> teams_in_trade)

let add_team_to_trade team_name trade_map =
  let team_list = teams_in_trade trade_map in
  if List.mem team_name team_list then trade_map
  else (team_name, []) :: trade_map

let remove_team_from_trade team_name trade_map =
  List.remove_assoc team_name trade_map

let is_player_in_trade player_name trade_map =
  List.exists
    (fun (team, incoming_players) ->
      List.mem player_name incoming_players)
    trade_map

let remove_player_from_trade player_name trade_map =
  List.map
    (fun (team, incoming_players) ->
      ( team,
        List.filter
          (fun player -> player <> player_name)
          incoming_players ))
    trade_map
