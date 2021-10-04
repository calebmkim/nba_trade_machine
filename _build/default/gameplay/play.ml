open Graphics 
open Data 
open Json_translation
open Button 

let _ = open_graph "";;

let show_welcome = 
  draw_string "Welcome to our NBA Trade Machine. Here are the instructions: ";
wait_next_event [Button_down]

type button = {text: string; ll: int*int;  ur: int*int}

let _ = clear_graph

let _ = open_graph ""

let y = size_y ()

let get_size_horz s = s|>text_size |> fst 
let get_size_vert s = s|>text_size |> snd

let get_new_point max_horz size_vert cur_x cur_y= 
  if (cur_y - size_vert) < 0 then (cur_x + max_horz+10, y - size_vert) else 
    ((cur_x),(cur_y - (size_vert)))

let get_max_size lst = lst|>List.sort 
(fun n1 n2 -> (get_size_horz n2 - get_size_horz n1))
|> List.hd |> get_size_horz

let print_team_name name cur_x  cur_y max_horz=
  let size_horz = get_size_horz name in 
  let size_vert = get_size_vert name in 
  let new_point = get_new_point max_horz size_vert cur_x cur_y in 
  let () = moveto (fst new_point) (snd new_point) in 
  let () = draw_string (name) in
  let () = moveto (fst new_point) (current_y ())
  in {text = name; ll = new_point; ur = (current_x () + size_horz , current_y () + size_vert)}

let show_team_roster name= 
  let _ = clear_graph in 
  let _ = open_graph "" in
  moveto 0 y; 
  let roster = get_roster_names_by_name name in 
  let max_horz = get_max_size (roster) in 
  let button_list = List.map (fun x -> print_team_name x (current_x ()) (current_y ()) max_horz) roster in 
  (ignore button_list);
  wait_next_event [Button_down]

let is_clicked x y button= 
print_endline (fst button.ur |> string_of_int);
((fst button.ll) <=x) && (fst button.ur >=x) && 
(snd button.ll <= y) && (snd button.ur >= y)

let handle_click st lst=
    print_endline "handling click";
    try let team = List.find (fun x -> is_clicked (st.mouse_x) (st.mouse_y) x) lst in 
    show_team_roster (team.text) 
  with 
  _ -> show_team_roster "Boton Celtics"

let show_team_list = 
  moveto 0 y;
  set_font "-linotype-avenir book-medium-r-normal--0-0-0-0-p-0-adobe-standard"; 
  let max_horz = get_max_size team_names in 
  let button_list = List.map (fun x -> print_team_name x (current_x ()) (current_y ()) max_horz)team_names in 
  let st = wait_next_event [Button_down] in 
  handle_click st button_list




(*(
let play_game = open_graph ""; print_endline "hi"

let main () =
  play_game



(* Execute the game engine. *)
let () = main ()
*)
