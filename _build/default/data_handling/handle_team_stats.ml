open Yojson.Basic.Util

let team_data = Yojson.Basic.from_file "data/team_stats.json"

let team_data_list = team_data |> to_list

(**[get_team_name x] takes in an 'Assoc type with a "Team Name" entry in
   its corresponding association list and returns the corresponding
   value of the "Team Name" entry*)
let get_team_name x =
  x |> to_assoc |> List.assoc "Team Name" |> to_string

(**[get_team_stats team_name] is the list of stats for team [team_name]*)
let get_team_stats team_name =
  let team_stats_json =
    List.find (fun x -> get_team_name x = team_name) team_data_list
  in
  team_stats_json |> to_assoc

let get_win_percentage team_name =
  let stats = team_name |> get_team_stats in
  let wins = List.assoc "Wins" stats |> to_int |> float_of_int in
  let losses = List.assoc "Losses" stats |> to_int |> float_of_int in
  wins /. (wins +. losses)
