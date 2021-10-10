open Graphics 
open Consts 

let _ = open_graph " 900x600"
let y = size_y ()

let open_graph_our_settings s= 
  let _ = open_graph " 900x600" in 
  set_font "-*-lucidatypewriter-*-*-*-*-*-*-*-*-*-150-*-*" 

let get_size_horz s = s|>text_size |> fst 
let get_size_vert s = s|>text_size |> snd

(**Determines whether, when we are drawing text of size [size_vert], whether 
we should go down and write on the next line or move to the right *)
let get_new_point max_horz size_vert cur_x cur_y= 
  if (cur_y - size_vert) < 0 then (cur_x + max_horz+10, y - size_vert) 
  else ((cur_x),(cur_y - (size_vert)))

(*[get_max_size] is the horizontal size of the largest text in [lst] given
the current text settings. Requires: [lst] is a nonempty list of strings.*)
let get_max_size lst = lst|>List.sort 
(fun n1 n2 -> (get_size_horz n2 - get_size_horz n1))
|> List.hd |> get_size_horz

let print_name name cur_x cur_y max_horz=
  let size_horz = get_size_horz name in 
  let size_vert = get_size_vert name in
  let new_point = get_new_point max_horz size_vert cur_x cur_y in 
  let () = moveto (fst new_point) (snd new_point) in 
  let () = draw_string (name) in
  let () = draw_rect (fst new_point) (snd new_point) size_horz size_vert in 
  let () = moveto (fst new_point) (current_y ())
  in (build_button name new_point size_horz size_vert)