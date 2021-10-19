type trade_map = (string * string list) list

type setting = {
  identifier : string;
  map : trade_map;
}
(**[setting] measures the setting of a current screen, with the
   [identifier] being the team/player name you are showing and
   [cur_teams] being the teams involved in the trade so far*)

let teams_in_trade tm = List.map fst tm

let make_trade_map teams = List.map (fun x -> (x, [])) teams

let get_name_setting t = t.identifier

let get_list_setting t = teams_in_trade t.map

let build_setting id team_list =
  { identifier = id; map = make_trade_map team_list }

type state =
  | Welcome
  | Teams
  | Roster of (string * bool)
  | Team_transition of string
  | Player of (string * bool)
  | Player_transition of string
  | FinalTeams
  | TradeResults
  | AlteredRoster of (string * string list * string list)
