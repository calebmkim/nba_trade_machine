open Data
open JsonTranslation
open Trademath
open Trademap

exception NoMatch of string

let per_difference p1 p2 =
  match (per p1, per p2) with
  | None, _ -> None
  | _, None -> None
  | Some x, Some y -> Some (x -. y)

let ws_diff p1 p2 =
  match (ws_per_48 p1, ws_per_48 p2) with
  | 0., _ -> None
  | _, 0. -> None
  | x, y -> Some ((x -. y) *. 89.)

let are_players_comparable p1 p2 =
  match (ws_diff p1 p2, per_difference p1 p2) with
  | None, _ -> false
  | _, None -> false
  | Some x, Some y -> Float.abs ((x +. y) /. 2.) <= 3.5

let make_swap_tmap p1 p2 =
  let t1 = get_team_of_player p1 in
  let t2 = get_team_of_player p2 in
  let empty_tmap = make_trade_map [ t1; t2 ] in
  empty_tmap |> add_player_to_trade p1 t2 |> add_player_to_trade p2 t1

let salary_trade_works tmap =
  let tms = teams_in_trade tmap in
  List.for_all (fun tm -> is_trade_viable tm tmap) tms

let does_trade_work p1 p2 =
  are_players_comparable p1 p2
  && salary_trade_works (make_swap_tmap p1 p2)

let on_different_teams p1 p2 =
  get_team_of_player p1 <> get_team_of_player p2

let trade_player p attribute_list =
  let leaders = get_leaders attribute_list in
  let new_player =
    try
      List.find
        (fun p' ->
          if on_different_teams p p' then does_trade_work p p'
          else false)
        leaders
    with
    | Not_found -> raise (NoMatch "Couldn't find trading partner")
  in
  make_swap_tmap p new_player
