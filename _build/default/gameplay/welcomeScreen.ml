open Graphics
open Button
open Trademap
open Common_functions
open State
open Assistant_gm_math

let handle_click st tm_button gm_button =
  if is_button_clicked tm_button st then "Teams"
  else if is_button_clicked gm_button st then "GM"
  else "Welcome"

let show_welcome t =
  let _ = start_state (size_y ()) in
  let _ =
    [ "Welcome to our NBA Trade Machine"; "What would you like to do?" ]
    |> make_button_list
  in
  let tm_button = make_button "Play the Trade Machine" in
  let gm_button = make_button "Play as a GM for an NBA team" in
  let st = wait_next_event [ Button_down ] in
  handle_click st tm_button gm_button
