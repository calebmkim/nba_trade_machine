open Button
open Trademap
open State

val team_transition : string -> trade_map -> state * trade_map
(** [team_options team trade_map] is the [(st, tm) ] tuple in which [st]
    is the the state the game should be in after the user's click. And
    [tm] is the trademap for the current trade given the user's click
    (in other words, it is an updated version of [trade_map]). It
    displays a graphics window which gives the user the option of what
    they want to do with the [team] while it waits for the user's click.
    The options that are displayed are whether the user wants to add
    [team] to the trade or examine [team]'s roster. Requires: [team] is
    the name of an NBA team*)

val player_transition : string -> trade_map -> state * trade_map
(** [player_transition player_name trade_map] is the [(st, tm)] tuple in
    which [st] is the state the game should be in after the user's
    click, and [tm] is the current trade map after the user's click (in
    other words, it is an updated version of [trade_map]). It displays a
    graphics window which asks the user if he wants to trade
    [player_name] AS WELL AS HIS STATS WHICH WE HAVEN'T ADDED YET while
    waiting for the user's click. Requires: [player_name] is the name of
    a current NBA player. *)
