open Trademap

type attribute = ThreePoint

exception NoMatch of string

val make_3pt_trade : string -> trade_map
