open Graphics
open Button
open States
open Common_functions

let handle_click st = Teams []

let show_welcome t =
  let _ = start_state (size_y ()) in
  let _ =
    [ "Welcome to our NBA Trade Machine"; "Click anywhere to start" ]
    |> make_button_list
  in
  let st = wait_next_event [ Button_down ] in
  handle_click st
