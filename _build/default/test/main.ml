open OUnit2
open Data
open Json_translation
open Gameplay
open Button
open Trademap
open Trade_math
open Handle_team_stats

(********************************************************************
  IMPORTANT: the following code is NOT our own. All credit should go to 
  the Cornell CS 3110 course staff.  
  Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(*This marks the end of the portion of code that is not ours. From this
  poitn forward, all of this code is ours. *)

let float_approx_eq f1 f2 = Float.abs (f1 -. f2) <= 0.001

let trademap_tuple_same (team1, players1) (team2, players2) =
  team1 = team2 && cmp_set_like_lists players1 players2

let rec sorted_trademaps_same tmap1 tmap2 =
  match (tmap1, tmap2) with
  | [], [] -> true
  | h :: t, [] -> false
  | [], h :: t -> false
  | h1 :: t1, h2 :: t2 ->
      trademap_tuple_same h1 h2 && sorted_trademaps_same t1 t2

let trademap_eqaul tmap1 tmap2 =
  let uniq1 = List.sort_uniq compare tmap1 in
  let uniq2 = List.sort_uniq compare tmap2 in
  List.length tmap1 = List.length uniq1
  && List.length tmap2 = List.length uniq2
  && sorted_trademaps_same uniq1 uniq2

let tmap_1 =
  make_trade_map
    [ "Chicago Bulls"; "Golden State Warriors"; "Utah Jazz" ]

let tmap1_incomplete =
  tmap_1
  |> add_player_to_trade "Stephen Curry" "Utah Jazz"
  |> add_player_to_trade "Draymond Green" "Chicago Bulls"
  |> add_player_to_trade "Zach LaVine" "Utah Jazz"
  |> add_player_to_trade "Joe Ingles" "Chicago Bulls"

let tmap1_complete =
  tmap_1
  |> add_player_to_trade "Klay Thompson" "Utah Jazz"
  |> add_player_to_trade "Eric Paschall" "Chicago Bulls"
  |> add_player_to_trade "Alex Caruso" "Golden State Warriors"
  |> add_player_to_trade "Damion Lee" "Chicago Bulls"
  |> add_player_to_trade "Udoka Azubuike" "Golden State Warriors"

let tmap_2 =
  make_trade_map
    [
      "Houston Rockets"; "Los Angeles Clippers"; "New Orleans Pelicans";
    ]

let tmap2_complete =
  tmap_2
  |> add_player_to_trade "John Wall" "Los Angeles Clippers"
  |> add_player_to_trade "Zion Williamson" "Houston Rockets"
  |> add_player_to_trade "Paul George" "New Orleans Pelicans"
  |> add_player_to_trade "Christian Wood" "Los Angeles Clippers"
  |> add_player_to_trade "Josh Hart" "Los Angeles Clippers"

(** [test_teams_in_trade name tmap expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [teams_in_trade trade_map]*)
let test_teams_in_trade name trade_map expected_output =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output
    (teams_in_trade trade_map)

let teams_in_trade_tests =
  [
    test_teams_in_trade "tmap1" tmap_1
      [ "Chicago Bulls"; "Golden State Warriors"; "Utah Jazz" ];
    test_teams_in_trade "tamp2" tmap_2
      [
        "Houston Rockets";
        "Los Angeles Clippers";
        "New Orleans Pelicans";
      ];
  ]

(** [test_valid_destinations name player tmap expected_output]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] with [get_valid_destinations player trade_map]*)
let test_valid_destinations name player trade_map expected_output =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output
    (get_valid_destinations player trade_map)

let valid_destinations_tests =
  [
    test_valid_destinations "Curry to Jazz or Bulls" "Stephen Curry"
      tmap_1
      [ "Utah Jazz"; "Chicago Bulls" ];
    test_valid_destinations "Rudy Gobert to Warriors or Bulls"
      "Rudy Gobert" tmap_1
      [ "Golden State Warriors"; "Chicago Bulls" ];
    test_valid_destinations "DJJ to Warriors or Jazzz"
      "Derrick Jones Jr." tmap_1
      [ "Golden State Warriors"; "Utah Jazz" ];
  ]

(** [test_trademap_list_function name f team trade_map expected_output]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] with [f team trade_map]*)
let test_trademap_list_function name f team trade_map expected_output =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output
    ~printer:(pp_list pp_string) (f team trade_map)

(** [test_players_acquiring] constructs an OUnit test named [name] that
    asserts the equality of [expected_output] with
    [players_acquiring team trade_map]*)
let test_players_acquiring name team trade_map expected_output =
  test_trademap_list_function name players_acquiring team trade_map
    expected_output

let players_acquiring_tests =
  [
    test_players_acquiring "tmap1 Jazz acquiring" "Utah Jazz"
      tmap1_complete [ "Klay Thompson" ];
    test_players_acquiring "tmap1 Warriors Acquiring"
      "Golden State Warriors" tmap1_complete
      [ "Alex Caruso"; "Udoka Azubuike" ];
    test_players_acquiring "tmap1 Bulls Acquiring" "Chicago Bulls"
      tmap1_complete
      [ "Damion Lee"; "Eric Paschall" ];
  ]

(** [test_players_losing] constructs an OUnit test named [name] that
    asserts the equality of [expected_output] with
    [players_losing team trade_map]*)
let test_players_losing name team trade_map expected_output =
  test_trademap_list_function name players_losing team trade_map
    expected_output

let players_losing_tests =
  [
    test_players_losing "tmap1 Jazz losing" "Utah Jazz" tmap1_complete
      [ "Udoka Azubuike"; "Eric Paschall" ];
    test_players_losing "tmap1 Warriors losing" "Golden State Warriors"
      tmap1_complete
      [ "Damion Lee"; "Klay Thompson" ];
    test_players_losing "tmap1 Bulls losing" "Chicago Bulls"
      tmap1_complete [ "Alex Caruso" ];
  ]

(** [test_valid_trade name tmap expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [valid_trade trade_map]*)
let test_valid_trade name trade_map expected_output =
  name >:: fun _ ->
  assert_equal expected_output ~printer:string_of_bool
    (valid_trade trade_map)

let valid_trade_tests =
  [
    test_valid_trade "Valid Trade" tmap1_complete true;
    test_valid_trade "Invalid Partially Complete" tmap1_incomplete false;
    test_valid_trade "Invalid Empty" tmap_1 false;
  ]

let trademap_tests =
  List.flatten
    [
      teams_in_trade_tests;
      valid_destinations_tests;
      players_acquiring_tests;
      players_losing_tests;
      valid_trade_tests;
    ]

(** [test_win_differential name team tmap expected_output] constructs an
    OUnit test named [name] that asserts the approximate equality of
    [expected_output] with [win_differential trade_map]*)
let test_win_differential name team trade_map expected_output =
  name >:: fun _ ->
  let wd = win_differential team trade_map in
  (*let _ = print_endline (string_of_float wd) in*)
  assert (float_approx_eq wd expected_output)

let win_differential_tests =
  [
    test_win_differential "Rockets in map 2" "Houston Rockets"
      tmap2_complete 13.073966;
    test_win_differential "Pelicans in map 2" "New Orleans Pelicans"
      tmap2_complete (Float.neg 6.262785);
    test_win_differential "Clippers in map 2" "Los Angeles Clippers"
      tmap2_complete (Float.neg 17.2889268);
  ]

let trade_math_tests = List.flatten [ win_differential_tests ]

(** [test_win_percent name team expected_output] constructs an OUnit
    test named [name] that asserts the approximate equality of
    [expected_output] with [get_win_percentage team]*)
let test_win_percent name team expected_output =
  name >:: fun _ ->
  let win_pct = get_win_percentage team in
  assert (float_approx_eq win_pct expected_output)

let win_percent_tests =
  [
    test_win_percent "Pacers win pct" "Indiana Pacers" 0.4722;
    test_win_percent "Nuggets win pct" "Denver Nuggets" 0.652777;
    test_win_percent "Kings win pct" "Sacramento Kings" 0.430555;
  ]

let team_stats_tests = List.flatten [ win_percent_tests ]

let suite =
  "test suite for Project"
  >::: List.flatten
         [ trademap_tests; trade_math_tests; team_stats_tests ]

let _ = run_test_tt_main suite
