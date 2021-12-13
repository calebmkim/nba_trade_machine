(**Functions to get general information information players but some
   information about teams, mainly about. This includes functions to get
   information on: rosters, stats of players,and teams of players. *)

val get_roster_names_by_name : string -> string list
(**[get_roster_strings s] is the names of the players onf team [s].
   Requires: [s] is the properly capitalized name of an NBA team. Ex:
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
(**[pts_per_48 p] is [Some x], where [x] is the points per 48 minutes
   for player [p] for the 2021 season. [None] if no such stat is
   available for the player. Requires: [p] is the name of a valid NBA
   player. *)

val per : string -> float option
(**[per p] is the is [Some x], where [x] is the PER for player [p] for
   the 2021 season. [None] if no such stat is available for the player.
   . Requires: [p] is the name of a valid NBA player. *)

val ast_pct : string -> float option
(**[ast_pct p] is [Some x], where [x] is the assist percent for player
   [p] for the 2021 season. [None] if no such stat is available for the
   player.Requires: [p] is the name of a valid NBA player. *)

val reb_pct : string -> float option
(**[reb_pct p] is [Some x], where [x] is the rebound percent for player
   [p] for the 2021 season. [None] if no such stat is available for the
   player. Requires: [p] is the name of a valid NBA player. *)

val drtg : string -> float option
(**[drtg p] is [Some x], where [x] is the defensive rating for player
   [p] for the 2021 season. [None] if no such stat is available for the
   player. Requires: [p] is the name of a valid NBA player. *)

val three_pt_pct : string -> float option
(**[three_pt_pct player] is [Some x], where [x] is the three point
   percent for player [p] for the 2021 season. [None] if no such stat is
   available for the player. Requires: [player] is the name of a current
   NBA player *)

val get_per_ratio : string list -> float
(**[get_per_ratio lst] is the weighted average PER of each player in
   [lst], to take into account minutes played. Ex: if [lst] has two
   player player A and player B, the weighted average is the sum of
   (minutes_played * per) for each player divided by the sum of
   (minutes_played). Requires: each string in [lst] is the name of a
   current NBA player*)

val get_leaders : string list -> string list
(**[get_leaders att_list] is a list of the current NBA players who have
   played over 982 minutes, sorted by the attributes listed in
   [att_list], assuming each attribute is weighted equally when sorting.
   For example, if [att_list] includes "Scoring" and "Athleticism" then
   it would get the average ranking of each player in "Athleticism" and
   "Athleticism", and rank and sort each player according to that
   average ranking. Requires: [att_list] is a list of properly spelled
   attribute names. The properly spelled attributes are: "3 Point
   Shooting"; "Defense"; "Rebounding"; "Playmaking"; "Scoring";
   "Athleticism" *)

val weighted_per_list : float list
(**[weighted_per_lst] is the weighted average per of each team*)

val team_names : string list
(**[team_names] are all 30 NBA teams (in alphabetical order)*)
