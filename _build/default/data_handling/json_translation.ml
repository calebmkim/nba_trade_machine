open Yojson.Basic.Util

type player = {name: string; team:int}

let season_info = Yojson.Basic.from_file "data/player_info.json"
let team_ids = "data/team_ids.json" |> Yojson.Basic.from_file |> to_assoc

let players = season_info |> to_assoc|> List.assoc "players" |> to_list 


let get_team p= 
p|> to_assoc |> List.assoc "tid" |> to_int 

(**[get_name p] is the name of player [p]*)
let get_name p = 
let p_info = p|> to_assoc in
try p_info |> List.assoc "name" |> to_string with 
Not_found -> let first = p_info |> List.assoc "firstName" |> to_string in 
let last = p_info |> List.assoc "lastName" |> to_string in 
first ^ " " ^ last 

(**[is_reg_season num s] determines whether [s] is the regular season for year [num]*)
let is_reg_season num s = (s|> to_assoc |> List.assoc "season" |> to_int) = num &&
(s|>to_assoc|>List.assoc "playoffs"|> to_bool |> not)

let get_ows p = 
  let p_info = p|> to_assoc in 
  p_info |> List.assoc "stats" |> to_list |> List.find (is_reg_season 2021) |> 
  to_assoc |> List.assoc "ows"


let make_player p = 
  let p_team = get_team p in 
  if (p_team >= 0) then {name = get_name p; team = p_team} else {name = ""; team = -1}
let league_build p_list = List.map (make_player) p_list |> List.filter (fun x -> x.name <> "")
let player_record = league_build players

let get_roster_by_int i = List.filter (fun x -> x.team = i) player_record 

let get_team_id team_name = (List.assoc team_name team_ids) |> to_int

let get_roster_by_name n = List.filter (fun x -> x.team = (get_team_id n)) player_record

let get_roster_strings n = List.map (fun x -> x.name) (get_roster_by_name n) 

let celtics = get_roster_by_name "Boston Celtics"

let hawks = get_roster_by_int 0

