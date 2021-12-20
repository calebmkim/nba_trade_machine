open Yojson.Basic.Util

let season_info = Yojson.Basic.from_file "data/player_info.json"

(**[players is the \[Yojson.Basic.t\] form of the player data]*)
let players = season_info |> to_assoc |> List.assoc "players" |> to_list

let team_ids =
  "data/team_ids.json" |> Yojson.Basic.from_file |> to_assoc

let team_names = List.map (fun x -> fst x) team_ids

let get_team_id team_name = List.assoc team_name team_ids |> to_int

(**[get_team_name_from_id i] is the team name of the name of an nba
   team. Requires: [i] >= 0, and [i] <= 29.*)
let get_team_name_from_id i =
  List.find (fun x -> get_team_id x = i) team_names

(**[get_team p] gets the team of the player represented by [p]*)
let get_team p = p |> to_assoc |> List.assoc "tid" |> to_int

(**[current_players] is the yojson list of current players in the NBA*)
let current_players =
  List.filter
    (fun x ->
      let tid = get_team x in
      tid >= 0 && tid <= 29)
    players

(**[get_name p] is the name of yojson "verion" of player [p]*)
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

(**[turn_to_float x] turns x into a float. If the stat happens to be a
   whole number, then we must turn it back into a float*)
let turn_to_float x =
  try to_int x |> float_of_int with
  | _ -> (
      try to_float x with
      | _ -> failwith "expected int or float")

(** [get_stat p stat] is the [stat] for player [p] *)
let get_rating p stat =
  try
    let p_info = p |> to_assoc in
    match p_info |> List.assoc "ratings" |> to_list with
    | [] -> None
    | h :: t ->
        let rtng = h |> to_assoc |> List.assoc stat in
        Some (rtng |> to_int |> float_of_int)
  with
  | _ -> None

let get_2021_season stat_lst =
  match List.filter (is_reg_season 2021) stat_lst with
  | [] -> failwith "Did not player last year"
  | [ h ] -> h
  | [ h1; h2 ] ->
      let min1 = h1 |> to_assoc |> List.assoc "min" in
      let min2 = h2 |> to_assoc |> List.assoc "min" in
      if min1 >= min2 then h1 else h2
  | _ -> failwith "too many teams"

let get_stat p stat =
  try
    let p_info = p |> to_assoc in
    Some
      (p_info |> List.assoc "stats" |> to_list |> get_2021_season
     |> to_assoc |> List.assoc stat |> turn_to_float)
  with
  | _ -> None

(*[get_team_in_yr p] is the team that the yojson version of player [p]
  played for in year [yr]. If the player did not play for a team in that
  year, then it is [None]*)
let get_team_in_yr p yr =
  try
    let p_info = p |> to_assoc in
    Some
      (p_info |> List.assoc "stats" |> to_list
      |> List.find (is_reg_season yr)
      |> to_assoc |> List.assoc "tid" |> to_int)
  with
  | _ -> None

(**[get_salary p] is the salary for player [p] for the current year*)
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

(** [get_per p] is the per of player [p] *)
let get_per p = get_stat p "per"

(** [get_3p_made p] is the 3 pointers made of player [p] *)
let get_3p_made p = get_stat p "tp"

(** [get_3p_attempted p] is the 3 pointers attempted of player [p] *)
let get_3p_attempted p = get_stat p "tpa"

(** [get_reb_pct] is the rebounding percentage of player [p] *)
let get_reb_pct p = get_stat p "trbp"

(** [get_reb_rating] is the rebounding rating of player [p] *)
let get_reb_rating p = get_rating p "reb"

(** [get_off_iq] is the offensive iq rating of player [p] *)
let get_off_iq p = get_rating p "oiq"

(** [get_ast_pct] is the assist percent of player [p] *)
let get_ast_pct p = get_stat p "astp"

(** [get_ast_pct] is the assist percent of player [p] *)
let get_pts p = get_stat p "pts"

let get_pts_per_48 p =
  match (get_pts p, get_minutes_played p) with
  | None, _ -> None
  | _, None -> None
  | Some p, Some m -> Some (p /. m *. 48.)

(** [get_drtg] is the assist percent of player [p] *)
let get_drtg p = get_stat p "drtg"

(** [get_jump] is the jump rating of player [p] *)
let get_jump p = get_rating p "jmp"

(** [get_spd] is the speed rating of player [p] *)
let get_speed p = get_rating p "spd"

let get_athleticism p =
  match (get_rating p "jmp", get_rating p "spd") with
  | None, _ -> None
  | _, None -> None
  | Some j, Some s -> Some (j +. s)

let get_reb_overall p =
  match (get_stat p "trbp", get_reb_rating p) with
  | None, None -> None
  | None, Some x -> None
  | Some x, None -> None
  | Some pct, Some rtng ->
      let combined = ((pct -. 10.) /. 10.) +. ((rtng -. 20.) /. 70.) in
      Some (combined /. 2.)

(** [get_player name] is the yojson version of player [p] *)
let get_player name =
  try List.find (fun x -> get_name x = name) players with
  | Not_found -> failwith "Player Not Found"

let return x = Some x

let ( >>= ) boxed_stat f =
  match boxed_stat with
  | None -> None
  | Some x -> f x

(** [get_3_pct] is the 3 point percent for the yojson version of player
    [p] *)
let get_3_pct p =
  p |> get_3p_made >>= fun x ->
  get_3p_attempted p >>= fun y ->
  if y = 0. then None else return (x /. y)

let stat p stat_type =
  let player_yojson = get_player p in
  get_stat player_yojson stat_type

let ows p = stat p "ows"

let dws p = stat p "dws"

let minutes_played p = stat p "min"

let salary p =
  let pl = get_player p in
  get_salary pl * 1000

let three_pt_pct p =
  let pl = get_player p in
  get_3_pct pl

let reb_pct p = stat p "trbp"

let reb_rating p =
  let pl = get_player p in
  get_reb_pct pl

let off_iq p =
  let pl = get_player p in
  get_off_iq pl

let ast_pct p = stat p "astp"

let per p = stat p "per"

let drtg p = stat p "drtg"

let pts_per_48 p =
  let pl = get_player p in
  get_pts_per_48 pl

let ath p =
  let pl = get_player p in
  match (get_speed pl, get_jump pl) with
  | None, _ -> None
  | _, None -> None
  | Some spd, Some jmp -> Some ((jmp +. spd) /. 2.)

let get_roster_names_by_name n =
  List.filter
    (fun pl -> get_team_name_from_id (get_team pl) = n)
    current_players
  |> List.map get_name

let get_prev_roster yr name =
  List.filter
    (fun x ->
      match get_team_in_yr x yr with
      | Some x -> name = get_team_name_from_id x
      | None -> false)
    players

let weighted_per_total name_lst =
  let player_lst =
    List.map
      (fun name -> List.find (fun p -> get_name p = name) players)
      name_lst
  in
  List.fold_left
    (fun acc elt ->
      match (get_minutes_played elt, get_per elt) with
      | Some min, Some per -> (per *. min) +. acc
      | _ -> acc)
    0. player_lst

let total_mins_played name_lst =
  let player_lst =
    List.map
      (fun name -> List.find (fun p -> get_name p = name) players)
      name_lst
  in
  List.fold_left
    (fun acc elt ->
      match get_minutes_played elt with
      | Some min -> min +. acc
      | None -> acc)
    0. player_lst

let get_per_ratio name_lst =
  weighted_per_total name_lst /. total_mins_played name_lst

let get_weighted_total team =
  List.fold_left
    (fun acc elt ->
      match (get_minutes_played elt, get_per elt) with
      | Some min, Some per -> (per *. min) +. acc
      | _ -> acc)
    0.
    (get_prev_roster 2021 team)

let get_total_mins_played team =
  List.fold_left
    (fun acc elt ->
      match get_minutes_played elt with
      | Some min -> min +. acc
      | None -> acc)
    0.
    (get_prev_roster 2021 team)

let get_weighted_per team =
  get_weighted_total team /. get_total_mins_played team

let weighted_per_list = List.map get_weighted_per team_names

let get_team_of_player p =
  try
    let player = List.find (fun x -> get_name x = p) players in
    get_team player |> get_team_name_from_id
  with
  | _ -> failwith "Cannot find player"

let at_least_3pt_makes n p =
  match get_3p_made p with
  | None -> false
  | Some x -> x >= n

let at_least_min_played n p =
  match get_minutes_played p with
  | None -> false
  | Some x -> x >= n

let div_float x y = float_of_int x /. float_of_int y

let ( //. ) = div_float

let get_percentile elt lst =
  let rec index_helper elt lst idx =
    match lst with
    | [] -> None
    | h :: t ->
        if h = elt then Some idx else index_helper elt t (idx + 1)
  in
  match index_helper elt lst 0 with
  | None -> None
  | Some idx ->
      Some
        (let plc = idx //. List.length lst in
         (plc *. -1.) +. 1.)

let convert_float_opt = function
  | None -> 0.
  | Some x -> x

let add_float x y = x +. convert_float_opt y

let get_total_percentile elt total_lst =
  List.fold_left
    (fun acc stat_lst ->
      let pct = get_percentile elt stat_lst in
      add_float acc pct)
    0. total_lst

let elligible_players =
  players
  |> List.filter (fun x ->
         let tid = get_team x in
         tid >= 0 && tid <= 29 && at_least_min_played 984. x)

let elligible_player_names =
  elligible_players |> List.map (fun x -> get_name x)

let get_leaders f =
  elligible_players
  |> List.sort (fun x y ->
         match (f x, f y) with
         | None, None -> 0
         | None, Some x -> -1
         | Some x, None -> 1
         | Some x, Some y ->
             if x > y then 1 else if x < y then ~-1 else 0)
  |> List.map (fun x -> get_name x)
  |> List.rev

let three_pt_pct_leaders = get_leaders get_3_pct

let three_pt_made_leaders = get_leaders get_3p_made

let reb_pct_leaders = get_leaders get_reb_pct

let ast_pct_leaders = get_leaders get_ast_pct

let pts_per_48_leaders = get_leaders get_pts_per_48

let athl_leaders = get_leaders get_athleticism

(*I don't want to reverse this list*)
let def_rating_leaders =
  elligible_players
  |> List.sort (fun x y ->
         match (get_drtg x, get_drtg y) with
         | None, None -> 0
         | None, Some x -> -1
         | Some x, None -> 1
         | Some x, Some y ->
             if x > y then 1 else if x < y then ~-1 else 0)
  |> List.map (fun x -> get_name x)

let overall_attributes_leaders lst_of_leaders =
  List.sort
    (fun x y ->
      let x_pct, y_pct =
        ( get_total_percentile x lst_of_leaders,
          get_total_percentile y lst_of_leaders )
      in
      if x_pct > y_pct then 1 else if x_pct < y_pct then ~-1 else 0)
    elligible_player_names
  |> List.rev

let three_pt_leaders =
  overall_attributes_leaders
    [ three_pt_pct_leaders; three_pt_made_leaders ]

let list_of_attributes attribute =
  match attribute with
  | "3 Point Shooting" -> three_pt_leaders
  | "Defense" -> def_rating_leaders
  | "Rebounding" -> reb_pct_leaders
  | "Playmaking" -> ast_pct_leaders
  | "Scoring" -> pts_per_48_leaders
  | "Athleticism" -> athl_leaders
  | _ -> failwith ("Couldn't interpret attribute: " ^ attribute)

let attributes_to_leaders attributes =
  List.map list_of_attributes attributes

let get_leaders att_list =
  let lst = attributes_to_leaders att_list in
  overall_attributes_leaders lst
