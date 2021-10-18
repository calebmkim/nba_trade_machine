open Button
open States

val team_options : setting -> state
(** [team_options setting] creates a graphics window and asks the user
    what they want to do with the team they have clicked, which
    corresponds to the identifier field of [setting]. Then once the user
    selects an option [team options setting] is the actual state that
    the game should be in after accepting the user's mouseclick is the
    state that the game should be in after accepting the user's mouse
    click. *)

val player_transition : string -> (string * string list) list -> state
(** [player_transition name trade_map] creates a graphics window and
    asks you which team you want to trade player who has name [name] to,
    given the [trade_map]. The only possible options are teams that are
    not on the list. Furthermore, if this player has already been
    selected to trade, then do nothing. Then once the user selects an
    team [player_transition name trade_map] represents the new state
    that the game should be in, given the user input. *)
