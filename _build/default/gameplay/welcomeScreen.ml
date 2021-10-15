open Graphics
open Button
open States

let handle_click st = Teams []

let show_welcome t =
  let _ = open_graph " 900x600" in
  let () = set_font "-*-lucidatypewriter-*-*-*-*-*-*-*-*-*-150-*-*" in
  draw_string
    "Welcome to our NBA Trade Machine. Click anywhere to start";
  let st = wait_next_event [ Button_down ] in
  handle_click st
