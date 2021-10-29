open Graphics
open Trademap

(**[open_graph_our_settings] opens a 900x600 graph.*)
let open_graph_our_settings s =
  let _ = open_graph " 900x600" in
  ()
(*set_font "-*-lucidatypewriter-*-*-*-*-*-*-*-*-*-150-*-*"*)

let start_state max_y =
  open_graph_our_settings "";
  moveto 0 max_y
