open Main_pages

val show_teams_gm : unit -> gmstate * bool
(**[show_teams_gm] shows all the 30 NBA teams for the user to pick to
   help manage. Based on the user's click, it returns
   [(new_state, go_back)] where [new_state] is the new state the game
   should be in given the user's click, and [go_back] is, based on the
   user's click, whether the game should to go back to the homepage. *)

val show_gm_team : string list -> string -> gmstate
(**[show_gm_team attributes team] shows all the roster for [team]. The
   [attributes] indicate the current attributes the user has selected to
   be the team needs. Based on the user's click, it returns the state of
   the game. *)

val pick_attributes : string -> string list -> gmstate * string list
(**[show_gm_team team attributes] shows all the attributes that the user
   has currently selected for [team], and shows the other options for
   the user to select [team]'s needs. The function returns the tuple
   [(state, new_attributes)] wherer [state] is the state the game should
   be in given the user's click, and [new_attributes] are the attributes
   that the user has now selected based on the user's click*)

val show_trade_recommendation : string -> string list -> gmstate
(**[show_gm_team player attributes] shows recommended trade for the team
   of [player] based on the attributes that [team] picked out. The
   function returns the [new_state] where [new_state] is the state the
   game should be in given the user's click*)
