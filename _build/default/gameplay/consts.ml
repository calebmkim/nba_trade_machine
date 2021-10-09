

type button = {text: string; ll: int*int;  ur: int*int}

type show_team_helper = {team: string; cur_teams: string list}

let get_name_transition t = t.team 

let get_list_transition t = t.cur_teams  

type state = Welcome | Teams of (string list) | Roster of show_team_helper 
|Team_transition of show_team_helper 

(*
let string_of_state st = match st with 
| Welcome -> "Welcome"
| Teams x -> "Teams"
| Roster x -> "Roster " ^ x
| Team_transition x -> "Team transition " ^ x.team *)
