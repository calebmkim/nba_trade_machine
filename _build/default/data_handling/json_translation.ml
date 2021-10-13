open Yojson.Basic.Util

type player = {
  name : string;
  team : int;
  stats : (string * float option) list;
}

let season_info = Yojson.Basic.from_file "data/player_info.json"

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

let get_ows p =
  try
    let p_info = p |> to_assoc in
    Some
      (p_info |> List.assoc "stats" |> to_list
      |> List.find (is_reg_season 2021)
      |> to_assoc |> List.assoc "ows" |> turn_to_float)
  with
  | _ -> None

(**[make_player p] makes a player record for [p] if p is a player
   currently in the NBA. If not, then nothing return a .*)
let make_player p =
  let p_team = get_team p in
  if p_team >= 0 then
    Some
      {
        name = get_name p;
        team = p_team;
        stats = [ ("ows", get_ows p) ];
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
  List.map (fun x -> x.name) (get_roster_by_name n)

let get_roster_names_by_int i =
  List.map (fun x -> x.name) (get_roster_by_int i)

let team_names = List.map (fun x -> fst x) team_ids

let get_team_name_from_id i =
  List.find (fun x -> get_team_id x = i) team_names

let get_team_of_player p =
  try
    let player = List.find (fun r -> r.name = p) player_record in
    player.team |> get_team_name_from_id
  with
  | _ -> failwith "Cannot find player"

let ows p =
  let player =
    try List.find (fun r -> r.name = p) player_record with
    | _ -> failwith "Cannot find player"
  in
  player.stats |> List.assoc "ows"

let player p =
  try List.find (fun x -> get_name x = p) players with
  | _ -> failwith "Cannot find player"
