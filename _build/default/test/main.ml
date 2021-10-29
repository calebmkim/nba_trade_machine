open OUnit2
open Data
open Json_translation

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

(** [get_roster_names_by_name_test name n expected_output] constructs an
    OUnit test named [name] that asserts the quality of
    [expected_output] with [get_roster_names_by_name n]. *)
let get_roster_names_by_name_test
    (name : string)
    (n : string)
    (expected_output : string list) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output
    (get_roster_names_by_name n)

(** [get_roster_names_by_int_test name n expected_output] constructs an
    OUnit test named [name] that asserts the quality of
    [expected_output] with [get_roster_names_by_int n]. *)
let get_roster_names_by_int_test
    (name : string)
    (n : int)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output
    (get_roster_names_by_int n)

(** [team_names_test name expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [team_names]. *)
let team_names_test (name : string) (expected_output : string list) :
    test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output team_names

(** [get_team_of_player_test name n expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [get_team_of_player n]. *)
let get_team_of_player_test
    (name : string)
    (n : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_team_of_player n)
    ~printer:String.escaped

(** [ows_test name n expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [ows n]. *)
let ows_test
    (name : string)
    (n : string)
    (expected_output : float option) : test =
  name >:: fun _ -> assert_equal expected_output (ows n)

(** [dws_test name n expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [dws n]. *)
let dws_test
    (name : string)
    (n : string)
    (expected_output : float option) : test =
  name >:: fun _ -> assert_equal expected_output (dws n)

(** [minutes_played_test name n expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [minutes_played n]. *)
let minutes_played_test
    (name : string)
    (n : string)
    (expected_output : float option) : test =
  name >:: fun _ -> assert_equal expected_output (minutes_played n)

(** [get_all_stats_test name n expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [get_all_stats n]. *)
let get_all_stats_test
    (name : string)
    (n : string)
    (expected_output : (string * float option) list) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output (get_all_stats n)

let json_tests =
  [
    get_roster_names_by_name_test "a" "Boston Celtics"
      (get_roster_names_by_name "Boston Celtics");
    get_roster_names_by_int_test "a" 0 (get_roster_names_by_int 0);
    get_roster_names_by_int_test "b" 10 (get_roster_names_by_int 10);
    get_roster_names_by_int_test "c" 29 (get_roster_names_by_int 29);
    team_names_test "team_names" team_names;
    get_team_of_player_test "a" "Kevin Love" "Cleveland Cavaliers";
    get_team_of_player_test "b" "LeBron James" "Los Angeles Lakers";
    get_team_of_player_test "c" "Jalen Johnson" "Atlanta Hawks";
    ows_test "a" "Jalen Johnson" None;
    ows_test "b" "LeBron James" (Some 3.);
    ows_test "c" "Kevin Love" (Some 0.3);
    dws_test "a" "Jalen Johnson" None;
    dws_test "b" "LeBron James" (Some 2.6);
    dws_test "c" "Kevin Love" (Some 0.6);
    minutes_played_test "a" "Jalen Johnson" None;
    minutes_played_test "b" "LeBron James" (Some 1504.);
    minutes_played_test "c" "Kevin Love" (Some 622.);
    get_all_stats_test "a" "Kevin Love" (get_all_stats "Kevin Love");
    get_all_stats_test "b" "Jalen Johnson"
      (get_all_stats "Jalen Johnson");
    get_all_stats_test "c" "LeBron James" (get_all_stats "LeBron James");
  ]

let suite = "test suite for JSON" >::: List.flatten [ json_tests ]

let _ = run_test_tt_main suite
