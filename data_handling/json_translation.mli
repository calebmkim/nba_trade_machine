val get_roster_names_by_name : string -> string list
(**[get_roster_strings s] is the names of the players onf team [s].
   Requires [s] is the properly capitalized name of an NBA team. Ex:
   "Boston Celtics" works, but "boston celtics" does not work.*)

val get_team_of_player : string -> string
(**[get_team_of_player p] is the team name of player [p]. Requires [p]
   is the valid first and last name (properly spaced) of an active NBA
   player*)

val ows : string -> float option
(**[ows p] is the ows for player [p] for the 2021 season. Requires: [p]
   is the name of a valid NBA player. *)

val dws : string -> float option
(**[dws p] is the dws for player [p] for the 2021 season. Requires: [p]
   is the name of a valid NBA player. *)

val minutes_played : string -> float option
(**[minutes_played p] is the minutes played by player [p] for the 2021
   season. Requires: [p] is the name of a valid NBA player. *)

val salary : string -> int
(**[salary p] is the salary of player [p] for the 2021 season. Requires:
   [p] is the name of a valid NBA player. *)

val pts_per_48 : string -> float option
(**[pts_per_48 p] is the points per 48 minutes for player [p] for the
   2021 season. Requires: [p] is the name of a valid NBA player. *)

val per : string -> float option
(**[per p] is the PER for player [p] for the 2021 season. Requires: [p]
   is the name of a valid NBA player. *)

val ast_pct : string -> float option
(**[ast_pct p] is the assist percentage for player [p] for the 2021
   season. Requires: [p] is the name of a valid NBA player. *)

val reb_pct : string -> float option
(**[reb_pct p] is the rebound percentage for player [p] for the 2021
   season. Requires: [p] is the name of a valid NBA player. *)

val drtg : string -> float option
(**[drtg p] is the defensive rating for player [p] for the 2021 season.
   Requires: [p] is the name of a valid NBA player. *)

val three_pt_pct : string -> float option
(*[[three_pt_pct player] is the three point percent for [player] last
  season. Requires: [player] is the name of a current NBA player *)

val get_per_ratio : string list -> float
(**[get_per_ratio lst] is the weighted average per of each player in
   [lst]. Requires: each string in [lst] is the name of a current NBA
   player*)

val get_leaders : string list -> string list
(*[[get_leaders att_list] are the overall league leaders in each of the
  attributes listed in [att_list], assuming each attribute is weighted
  equally. Requires: [att_list] is a list of properly spelled attribute
  names *)

val weighted_per_list : float list
(**[weighted_per_lst] is the weighted average per of each team*)

val team_names : string list
(**[team_names] are all 30 NBA teams (in alphabetical order)*)
