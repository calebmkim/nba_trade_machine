open Owl
open Data

let per_lst = Json_translation.weighted_per_list

let win_pct_lst = Handle_team_stats.team_win_pct_list

let per_arr = Array.of_list per_lst

let win_pct_arr = Array.of_list win_pct_lst

let per_mat = Mat.of_array per_arr 1 30

let win_pct_mat = Mat.of_array win_pct_arr 1 30

let per_constant, per_slope = Linalg.D.linreg per_mat win_pct_mat
