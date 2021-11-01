open Yojson.Basic.Util

type player = {
  name : string;
  team : int;
  salary : int;
  stats : (string * float option) list;
}

let season_info = Yojson.Basic.from_file "data/player_info.json"

let salary_info =
  Yojson.Basic.from_file "data/nbasalarydata.json" |> to_list

let get_team_info team_name =
  List.find
    (fun json_data ->
      let data = json_data |> to_assoc in
      let name = List.assoc "Name" data |> to_string in
      team_name = name)
    salary_info

let get_cap_differential team_name =
  let info = get_team_info team_name |> to_assoc in
  let diff = List.assoc "Luxury Tax Difference" info in
  diff |> to_string |> int_of_string

let team_ids =
  "data/team_ids.json" |> Yojson.Basic.from_file |> to_assoc

(**[players is the Yojson.Basic.t form of the player data]*)
let players = season_info |> to_assoc |> List.assoc "players" |> to_list

(**[get_team p] gets the team of the player represented by [p]*)
let get_team p = p |> to_assoc |> List.assoc "tid" |> to_int

(**[get_name p] is the name of player [p]*)
let get_name p =
  let p_info = p |> to_assoc in
  try p_info |> List.assoc "name" |> to_string with
  | Not_found ->
      let first = p_info |> List.assoc "firstName" |> to_string in
      let last = p_info |> List.assoc "lastName" |> to_string in
      first ^ " " ^ last

(**[is_reg_season num s] determines whether [s] is the regular season
   for year [num]*)
let is_reg_season num s =
  s |> to_assoc |> List.assoc "season" |> to_int = num
  && s |> to_assoc |> List.assoc "playoffs" |> to_bool |> not

(**If the stat happens to be a whole number, then we must turn it back
   into a float*)
let turn_to_float x =
  try to_int x |> float_of_int with
  | _ -> (
      try to_float x with
      | _ -> failwith "expected int or float")

(** [get_stat p stat] is the [stat] for player [p] *)
let get_stat p stat =
  try
    let p_info = p |> to_assoc in
    Some
      (p_info |> List.assoc "stats" |> to_list
      |> List.find (is_reg_season 2021)
      |> to_assoc |> List.assoc stat |> turn_to_float)
  with
  | _ -> None

let get_salary p =
  let p_info = p |> to_assoc in
  List.assoc "contract" p_info
  |> to_assoc |> List.assoc "amount" |> to_int

(** [get_ows p] is the offensive win shares for player [p] *)
let get_ows p = get_stat p "ows"

(** [get_dws p] is the defensive win shares for player [p] *)
let get_dws p = get_stat p "dws"

(** [get_minutes_played p] is the minutes played by player [p] *)
let get_minutes_played p = get_stat p "min"

(**[make_player p] makes a player record for [p] if p is a player
   currently in the NBA. If not, then nothing return a .*)
let make_player p =
  let p_team = get_team p in
  if p_team >= 0 then
    Some
      {
        name = get_name p;
        team = p_team;
        salary = 0;
        stats =
          [
            ("ows", get_ows p);
            ("dws", get_dws p);
            ("min", get_minutes_played p);
          ];
      }
  else None

(**[get_player_record] is used as a helper function to get the actual
   value of a player from a player option*)
let get_player_record p =
  match p with
  | Some v -> v
  | None -> failwith "Player Doesn't Exist"

(**[league_build p_list] is the list of all NBA players currently
   playing*)
let league_build p_list =
  List.map make_player p_list
  |> List.filter (fun x -> x <> None)
  |> List.map get_player_record

(**[player_record] is a list of all the nba players currently playing*)
let player_record = league_build players

(**[get_roster_by_int i] is the list of players on the team
   corresponding to team_id [i]. Requires: 0<=i<=29*)
let get_roster_by_int i =
  List.filter (fun x -> x.team = i) player_record

(**[get_team_id team_name] is the team_id of the name of an nba team.
   Requires: [team_name] is the name of an NBA team, properly
   capitalized. Ex: "Boston Celtics". would work, but "boston celtics"
   would not work.*)
let get_team_id team_name = List.assoc team_name team_ids |> to_int

(**[get_roster_by_name n] is the list of players on the team
   corresponding to team name [n]. Requires:[n] is the name of a
   properly capitalized NBA team. *)
let get_roster_by_name n =
  List.filter (fun x -> x.team = get_team_id n) player_record

let get_roster_names_by_name n =
  try List.map (fun x -> x.name) (get_roster_by_name n) with
  | _ -> failwith "Team Does Not Exist"

let get_roster_names_by_int i =
  try List.map (fun x -> x.name) (get_roster_by_int i) with
  | _ -> failwith "Team Does Not Exist"

let team_names = List.map (fun x -> fst x) team_ids

let get_team_name_from_id i =
  List.find (fun x -> get_team_id x = i) team_names

let get_team_of_player p =
  try
    let player = List.find (fun r -> r.name = p) player_record in
    player.team |> get_team_name_from_id
  with
  | _ -> failwith "Cannot find player"

let stat p stat_type =
  let player =
    try List.find (fun r -> r.name = p) player_record with
    | _ -> failwith "Cannot find player"
  in
  player.stats |> List.assoc stat_type

let ows p = stat p "ows"

let dws p = stat p "dws"

let minutes_played p = stat p "min"

let player p =
  try List.find (fun x -> get_name x = p) players with
  | _ -> failwith "Cannot find player"

(*TINSAE IMPLEMENT THIS Create a new Json translation method that gives
  all of the stats in a string* float option list*)
let get_all_stats p =
  try
    (List.find (fun player -> player.name = p) player_record).stats
  with
  | _ -> failwith "Cannot find player"

let salary p =
  let pl = player p in
  get_salary pl * 1000
