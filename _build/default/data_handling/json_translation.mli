val get_roster_names_by_name : string -> string list
(**[get_roster_strings s] is the names of the players onf team [s].
   Requires [s] is the properly capitalized name of an NBA team. Ex:
   "Boston Celtics" works, but "boston celtics" does not work.*)

val get_roster_names_by_int : int -> string list
(**[get_roster_names_by_int i] is the names of the players on team who
   has team id of[i]. Requires: 0<=i<=29 .*)

val team_names : string list
(**[team_names] are all 30 NBA teams in alphabetical order by city Name*)

val get_team_of_player : string -> string
(**[get_team_of_player p] is the team name of player [p]. Requires [p]
   is the valid first and last name (properly spaced) of an active NBA
   player*)

val ows : string -> float option
(**[ows p] is the ows for player names [p] for the 2021 season.
   Requires: [p] is the name of a valid NBA player. *)

val player : string -> Yojson.Basic.t
