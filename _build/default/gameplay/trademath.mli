(** Functions that help to perform analysis on viability of trade and
    which team benfited in a trade*)

open Trademap

val win_differential : string -> trade_map -> float
(**[win_differential team tmap] is the predicted amount of wins gained
   or lossed by [team] if the trade represented by [tmap] were to be
   executed. This is the combined win shares of all the players acquired
   by [team] if each player were to play all 82 games, minus the
   combined win shares of all the players departing [team] if each
   player were to play all 82 games, taking into acount an average
   replacement player if there is an imbalance between number of players
   receiving and departing (for example, if a team were to trade 2
   players but acquire only 1, it would not be a good analysis to just
   add up the stats, since obviously 2 players are going to have higher
   combined stats than 1. We take this into account by simulating an
   "average" player on [team]) Requires: [team] is a team inovlved in
   the trade indicated by [tmap]. *)

val is_trade_viable : string -> trade_map -> bool
(**[is_trade_viable team tmap] is whether the trade indicated by [tmap]
   is viable for [team] given the NBA's slary cap rules. Requires:
   [team] is a team inovlved in the trade indicated by [tmap]. *)

val win_diff_per : string -> trade_map -> float
(**[win_diff_per team tmap] is the predicted win differential for [team]
   if the trade represented by [trade_map] were to occur. Requires:
   [team] is a team inovlved in the trade indicated by [tmap]. (If you
   want more information about how we calculate this, please contact us)*)

val ws_per_48 : string -> float
(**[ws_per_48 player_name] is the win shares per 48 minute for
   [player_name]. Requires: [player_name] is the name of a current NBA
   player *)
