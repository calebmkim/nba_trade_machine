open Yojson.Basic.Util

type player = {name: string; team:int}
type team = player list 

type t = (string * team) list 


let season_info = Yojson.Basic.from_file "player_info.json"
let team_ids = "team_ids.json" |> Yojson.Basic.from_file |> to_assoc

let players = season_info |> to_assoc|> List.assoc "players" |> to_list 


let get_team p= 
p|> to_assoc |> List.assoc "tid" |> to_int 

let get_name p = 
let p_info = p|> to_assoc in
try p_info |> List.assoc "name" |> to_string with 
Not_found -> let first = p_info |> List.assoc "firstName" |> to_string in 
let last = p_info |> List.assoc "lastName" |> to_string in 
first ^ " " ^ last 

let make_player p = 
  let p_team = get_team p in 
  if (p_team >= 0) then {name = get_name p; team = p_team} else {name = ""; team = -1}



let league_build p_list = List.map (make_player) p_list |> List.filter (fun x -> x.name <> "")
let player_record = league_build players

let get_roster_by_int i = List.filter (fun x -> x.team = i) player_record 

let get_team_id team_name = (List.assoc team_name team_ids) |> to_int

let get_roster_by_name n = List.filter (fun x -> x.team = (get_team_id n)) player_record