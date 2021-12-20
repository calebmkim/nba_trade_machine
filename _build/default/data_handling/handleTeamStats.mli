(** Functions to get information about team stats *)

val get_win_percentage : string -> float
(**[get_win_percentage team_name] is the win percentage of [team_name]
   given as a float from 0.0 to 1.0 . Requires: [team_name] is a valid
   NBA team name. *)

val team_win_pct_list : float list
(**[team_win_pct_lst] is the list of win percentage of each team,
   ordered by alphabetical order (e.g the first number in the list is
   the Atlanta Hawks' win percent, and the last number in the list is
   the Washington Wizards' win percent)*)
