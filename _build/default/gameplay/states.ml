type setting = {
  identifier : string;
  cur_teams : string list;
}
(**[setting] measures the setting of a current screen, with the
   [identifier] being the team/player name you are showing and
   [cur_teams] being the teams involved in the trade so far*)

let get_name_setting t = t.identifier

let get_list_setting t = t.cur_teams

let build_setting id team_list =
  { identifier = id; cur_teams = team_list }

type trade_map = (string * string list) list

type state =
  | Welcome
  | Teams of string list
  | Roster of (setting * bool)
  | Team_transition of setting
  | Player of (setting * bool)
  | Player_transition of string
  | FinalTeams of (string * string list) list
  | TradeResults of (string * string list) list
  | AlteredRoster of (string * string list * string list)
