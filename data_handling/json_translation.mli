(**[get_roster_strings s] is the names of the players onf team [s]. Requires [s] is 
the properly capitalized name of an NBA team. Ex: "Boston Celtics" works, but 
"boston celtics" does not work.*)
val get_roster_names_by_name: string -> string list 

(**[get_roster_names_by_int i] is the names of the players on team who has 
team id of[i]. Requires: 0<=i<=29 .*)
val get_roster_names_by_int: int -> string list 

(**[team_names] are all 30 NBA teams in alphabetical order by city Name*)
val team_names: string list 

(**[get_team_of_player p] is the team name of player [p]*)
val get_team_of_player: string -> string 