type setting = {
  identifier : string;
  cur_teams : string list;
}
(** setting represents an identifier i.e a player or team name, along
    with the teams that the user has currently selected to be in the
    trade*)
val get_name_setting: setting -> string 
(** [get_name_setting s] is the identifier associated with [s]*)

val get_list_setting: setting -> string list 
(**[get_list_setting s] is the list of teams in the trade so far, given [s]*)

val build_setting: string -> string list -> setting 
(**[build_setting s lst] builds a setting with the identifier equal to 
[s] and the list equal to [lst]*)

type trade_map = (string * string list) list
(**represents the current trade being processed*)

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
(**this represents the different states the game may be in*)