open States

val show_team_roster :
  setting -> bool -> (string * string list) list -> state
(**show_team_roster [settings teams_picked trade_map] is the state that
   the machine should be in, given the current [trade map], [settings]
   amd whether [teams_picked] or not, and the user's mouse click. At the
   same time, it displays the roster of the NBA team as indicated by the
   identifier in [setting]. *)

val show_team_list : string list -> state
(**[show_team_list lst]is the state that the machine should be in, given
   the user's mouseclick input, as well as the current list of teams in
   the trade [lst]. It displays all 30 NBA teams, as well a banner that
   reads teams currently in trade, which displays the teams the user has
   currentlly selected to be in the trade, as identified by [lst].
   Finally it displays a button allowing the user to go to finalize, or
   lock in, the teams that are currently selected to be in the trade. *)

val show_final_teams :
  (string * string list) list -> state * (string * string list) list
(**[show_final_teams trade_map] *)
