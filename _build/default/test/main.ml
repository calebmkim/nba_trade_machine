open OUnit2
open Data
open Json_translation
open Handle_salary_data
open Handle_team_stats
open Gameplay
open Button
open Trademap
open Trade_math

(********************************************************************
  The following code is NOT our own. All credit should go to 
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
  point forward, all of this code is ours. *)

let float_approx_eq f1 f2 = Float.abs (f1 -. f2) <= 0.01

let float_option_eq f1 f2 =
  match (f1, f2) with
  | None, None -> true
  | Some x, Some y -> float_approx_eq x y
  | None, Some y -> false
  | Some y, None -> false

let string_of_float_option = function
  | None -> "None"
  | Some x -> string_of_float x

(** [player_stat_test f name n expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [f n]. *)
let player_stat_test
    (f : string -> float option)
    (name : string)
    (n : string)
    (expected_output : float option) : test =
  name >:: fun _ ->
  assert_equal expected_output (f n) ~printer:string_of_float_option

(** [player_stat_approx_test f name n expected_output] constructs an
    OUnit test named [name] that asserts the quality of
    [expected_output] with [f n], with a margin for error. *)
let player_stat_approx_test
    (f : string -> float option)
    (name : string)
    (n : string)
    (expected_output : float option) : test =
  name >:: fun _ -> assert (float_option_eq expected_output (f n))

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
let ows_test = player_stat_test ows

(** [dws_test name n expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [dws n]. *)
let dws_test = player_stat_test dws

(** [minutes_played_test name n expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [minutes_played n]. *)
let minutes_played_test = player_stat_test minutes_played

(** [salary_test name n expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [salary n]. *)
let salary_test (name : string) (n : string) (expected_output : int) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (salary n) ~printer:string_of_int

(** [pts_per_48_test name n expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [pts_per_48 n]. *)
let pts_per_48_test = player_stat_approx_test pts_per_48

(** [per_test name n expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [per n]. *)
let per_test = player_stat_test per

(** [ast_pct_test name n ast_pct] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [ast_pct n]. *)
let ast_pct_test = player_stat_test ast_pct

(** [reb_pct_test name n ast_pct] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [reb_pct n]. *)
let reb_pct_test = player_stat_test reb_pct

(** [drtg_test name n ast_pct] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [drtg n]. *)
let drtg_test = player_stat_test drtg

(** [three_pt_pct_test name n ast_pct] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [three_pt_pct n]. *)
let three_pt_pct_test = player_stat_approx_test three_pt_pct

(** [member_test name player team expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [List.mem player (get_roster_names_by_name team)]. *)
let member_test
    (name : string)
    (player : string)
    (team : string)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (List.mem player (get_roster_names_by_name team))
    ~printer:string_of_bool

(** [per_ratio_test name player_list expected_output] constructs an
    OUnit test named [name] that asserts the quality of
    [expected_output] with [get_per_ratio player_list]. *)
let per_ratio_test
    (name : string)
    (player_list : string list)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert (float_approx_eq expected_output (get_per_ratio player_list))

let roster_tests =
  [
    member_test "Steph Curry on Warriors" "Stephen Curry"
      "Golden State Warriors" true;
    member_test "Terence Davis on Kings" "Terence Davis"
      "Sacramento Kings" true;
    member_test "Tim Hardaway Jr. on Mavs" "Tim Hardaway Jr."
      "Dallas Mavericks" true;
    member_test "Josh Hart on Pelicans" "Josh Hart"
      "New Orleans Pelicans" true;
    member_test "Kawhi Leonard Not on Spurs" "Kawhi Leonard"
      "San Antonio Spurs" false;
    member_test "Enes Kanter not on Thunder" "Enes Kanter"
      "Oklahoma City Thunder" false;
    member_test "Devin Booker not on Blazers" "Devin Booker"
      "Portnland Trail Blazers" false;
  ]

let ws_tests =
  [
    ows_test "Devonte' Graham ows" "Devonte' Graham" (Some 2.5);
    ows_test "Zion ows" "Zion Williamson" (Some 7.1);
    ows_test "Saddiq' Bey ows" "Saddiq Bey" (Some 1.6);
    ows_test "James Harden ows" "James Harden" (Some 4.5);
    dws_test "Devonte' Graham dws" "Devonte' Graham" (Some 0.9);
    dws_test "Zion dws" "Zion Williamson" (Some 1.6);
    dws_test "Saddiq Bey dws" "Saddiq Bey" (Some 1.7);
    dws_test "James Harden dws" "James Harden" (Some 1.5);
    ows_test "Cade Cunningham ows" "Cade Cunningham" None;
    dws_test "Cade Cunningham dws" "Cade Cunningham" None;
    ows_test "Jalen Johnson ows" "Jalen Johnson" None;
    ows_test "Lebron ows" "LeBron James" (Some 3.);
    ows_test "Kevin Love ows" "Kevin Love" (Some 0.3);
    dws_test "Jalen Johnson dws" "Jalen Johnson" None;
    dws_test "Lebron dws" "LeBron James" (Some 2.6);
    dws_test "Kevin Love dws" "Kevin Love" (Some 0.6);
  ]

let min_played_tests =
  [
    minutes_played_test "Jalen Johnson mins" "Jalen Johnson" None;
    minutes_played_test "Lebron mins" "LeBron James" (Some 1504.);
    minutes_played_test "K Love mins" "Kevin Love" (Some 622.);
    minutes_played_test "AD mins" "Anthony Davis" (Some 1162.);
    minutes_played_test "Donte DiVincenzo mins" "Donte DiVincenzo"
      (Some 1814.);
    minutes_played_test "Collin Sexton mins" "Collin Sexton"
      (Some 2115.);
  ]

let team_of_player_tests =
  [
    get_team_of_player_test "K Love team" "Kevin Love"
      "Cleveland Cavaliers";
    get_team_of_player_test "Lebron team" "LeBron James"
      "Los Angeles Lakers";
    get_team_of_player_test "Jalen Johnson Team" "Jalen Johnson"
      "Atlanta Hawks";
    get_team_of_player_test "Jalen Green Team" "Jalen Green"
      "Houston Rockets";
    get_team_of_player_test "Jimmy Butler Team" "Jimmy Butler"
      "Miami Heat";
  ]

let salary_tests =
  [
    salary_test "D Russell Salary" "D'Angelo Russel" 30025000;
    salary_test "Terry Rozier Salary" "Terry Rozier" 17900000;
    salary_test "Christian Wood Salary" "Christian Wood" 13675000;
  ]

let pts_per_48_tests =
  [
    pts_per_48_test "B Ingram pts/48" "Brandon Ingram" (Some 33.2537);
    pts_per_48_test "Derrick Jones Jr. pts/48" "Derrick Jones Jr."
      (Some 14.42185);
    pts_per_48_test "Richaul Holmes pts/48" "Richaun Holmes"
      (Some 23.407);
    pts_per_48_test "Scotttie Barnes pts/48" "Scottie Barnes" None;
  ]

let per_tests =
  [
    per_test "Maxi Kleber PER" "Maxi Kleber" (Some 10.6);
    per_test "PG13 per" "Paul George" (Some 20.5);
    per_test "KAT per" "Karl-Anthony Towns" (Some 23.1);
    per_test "Buddy Hield per" "Buddy Hield" (Some 12.8);
    per_test "Josh Giddey per" "Josh Giddey" None;
  ]

let ast_pct_tests =
  [
    ast_pct_test "Duncan Robinson Ast Pct" "Duncan Robinson" (Some 8.3);
    ast_pct_test "Kelly Oubre Ast Pct" "Kelly Oubre Jr." (Some 6.5);
    ast_pct_test "Zach Lavine Ast Pct" "Zach LaVine" (Some 23.4);
    ast_pct_test "Franz Wagner Ast Pct" "Franz Wagner" None;
  ]

let reb_pct_tests =
  [
    reb_pct_test "Serge Ibaka reb pct" "Serge Ibaka" (Some 16.2);
    reb_pct_test "K Nunn reb pct" "Kendrick Nunn" (Some 6.2);
    reb_pct_test "RJ Barrett reb pct" "R.J. Barrett" (Some 8.9);
    reb_pct_test "Davion Mitchell reb pct" "Davion Mitchell" None;
  ]

let drtg_tests =
  [
    drtg_test "Pascal Siakam def rating" "Pascal Siakam" (Some 113.);
    drtg_test "CP3 def rating" "Chris Paul" (Some 111.);
    drtg_test "Ricky Rubio def rating" "Ricky Rubio" (Some 115.);
    drtg_test "Zaire Williams def rating" "Ziaire Williams" None;
  ]

let three_pt_pct_tests =
  [
    three_pt_pct_test "Porzingis 3 pct" "Kristaps Porzingis"
      (Some 0.37596);
    three_pt_pct_test "Lowry 3 pct" "Kyle Lowry" (Some 0.39577);
    three_pt_pct_test "Jerami Grant 3 pct" "Jerami Grant"
      (Some 0.349544);
    three_pt_pct_test "Moses Moody 3 pct" "Moses Moody" None;
  ]

let four_player_list =
  [ "Trey Lyles"; "Luke Kennard"; "Josh Giddey"; "Dario Saric" ]

let per_ratio_tests =
  [ per_ratio_test "Test PER Ratio" four_player_list 13.07716 ]

let json_tests =
  List.flatten
    [
      roster_tests;
      ws_tests;
      min_played_tests;
      team_of_player_tests;
      pts_per_48_tests;
      per_tests;
      ast_pct_tests;
      reb_pct_tests;
      drtg_tests;
      three_pt_pct_tests;
      per_ratio_tests;
    ]

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

let tmap_3 =
  make_trade_map [ "New Orleans Pelicans"; "Dallas Mavericks" ]

let tmap3_complte =
  tmap_3
  |> add_player_to_trade "Kristaps Porzingis" "New Orleans Pelicans"
  |> add_player_to_trade "Josh Hart" "Dallas Mavericks"

let tmap_4 =
  make_trade_map [ "Boston Celtics"; "Portland Trail Blazers" ]

let tmap4_partial =
  tmap_4
  |> add_player_to_trade "Jayson Tatum" "Portland Trail Blazers"
  |> add_player_to_trade "C.J. McCollum" "Boston Celtics"
  |> add_player_to_trade "Anfernee Simons" "Boston Celtics"

let tmap4_complete =
  tmap4_partial
  |> add_player_to_trade "Al Horford" "Portland Trail Blazers"

let tmap5_partial =
  make_trade_map [ "Oklahoma City Thunder"; "Orlando Magic" ]
  |> add_player_to_trade "Jonathan Isaac" "Oklahoma City Thunder"
  |> add_player_to_trade "Derrick Favors" "Orlando Magic"
  |> add_player_to_trade "Markelle Fultz" "Oklahoma City Thunder"

let tmap5_complete =
  tmap5_partial
  |> add_player_to_trade "Gary Harris" "Oklahoma City Thunder"
  |> add_player_to_trade "Franz Wagner" "Oklahoma City Thunder"
  |> add_player_to_trade "Mohamed Bamba" "Oklahoma City Thunder"
  |> add_player_to_trade "Jalen Suggs" "Oklahoma City Thunder"

let invalid_tmap_ex =
  make_trade_map [ "Boston Celtics"; "Utah Jazz" ]
  |> add_player_to_trade "Jayson Tatum" "Utah Jazz"
  |> add_player_to_trade "Rudy Gobert" "Boston Celtics"
  |> remove_player_from_trade "Rudy Gobert"

let removed_celtics_from_trade =
  invalid_tmap_ex |> remove_team_from_trade "Boston Celtics"

(** [test_in_remaining_players name player team trade_map expected_output]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] with
    [List.mem player (get_remaining_players team altered_rosters)]*)
let test_is_player_in_trade name player trade_map expected_output =
  name >:: fun _ ->
  assert_equal expected_output ~printer:string_of_bool
    (is_player_in_trade player trade_map)

let is_player_in_trade_tests =
  [
    test_is_player_in_trade "John Wall in tmap 2" "John Wall"
      tmap2_complete true;
    test_is_player_in_trade "Josh Hart in tmap 2" "Josh Hart"
      tmap2_complete true;
    test_is_player_in_trade "Kawhi Leonard not in tmap 2"
      "Kawhi Leonard" tmap2_complete false;
    test_is_player_in_trade " Jalen Green not in tmap 2" "Jalen Green"
      tmap2_complete false;
    test_is_player_in_trade "Gary Harris in tmap 5" "Gary Harris"
      tmap5_complete true;
    test_is_player_in_trade "Franz Wagner in tmap 5" "Franz Wagner"
      tmap5_complete true;
    test_is_player_in_trade "Josh Gidden not in tmap 5" "Josh Giddey"
      tmap5_complete false;
  ]

let team_in_changed_rosters team altered_rosters =
  try
    List.find
      (fun (team_name, remaining, acquiring) -> team = team_name)
      altered_rosters
  with
  | Not_found -> failwith "should not happen"

let get_receiving_players team new_rosters =
  let _, _, incoming = team_in_changed_rosters team new_rosters in
  incoming

let get_remaining_players team new_rosters =
  let _, remaining, _ = team_in_changed_rosters team new_rosters in
  remaining

(** [test_in_remaining_players name player team trade_map expected_output]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] with
    [List.mem player (get_remaining_players team altered_rosters)]*)
let test_in_remaining_players name player team trade_map expected_output
    =
  name >:: fun _ ->
  let altered_rosters = change_rosters trade_map in
  assert_equal expected_output ~printer:string_of_bool
    (List.mem player (get_remaining_players team altered_rosters))

(** [test_in_receiving_players name player team trade_map expected_output]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] with
    [List.mem player (get_remaining_players team altered_rosters)]*)
let test_in_receiving_players name player team trade_map expected_output
    =
  name >:: fun _ ->
  let altered_rosters = change_rosters trade_map in
  assert_equal expected_output ~printer:string_of_bool
    (List.mem player (get_receiving_players team altered_rosters))

let change_rosters_tests =
  [
    test_in_remaining_players
      "Tmap2 change_rosters, Wall no longer on rockets " "John Wall"
      "Houston Rockets" tmap2_complete false;
    test_in_remaining_players
      "Tmap2 change_rosters, PG13 no longer on clippers " "Paul George"
      "Los Angeles Clippers" tmap2_complete false;
    test_in_remaining_players
      "Tmap2 change_rosters, BI still on pelicans " "Brandon Ingram"
      "New Orleans Pelicans" tmap2_complete true;
    test_in_remaining_players
      "Tmap2 change_rosters, Kawhi still on clippers " "Kawhi Leonard"
      "Los Angeles Clippers" tmap2_complete true;
    test_in_receiving_players
      "Tmap2 change_rosters, Christian Wood to clippers "
      "Christian Wood" "Los Angeles Clippers" tmap2_complete true;
    test_in_receiving_players
      "Tmap2 change_rosters, Zion Williamson to rockets "
      "Zion Williamson" "Houston Rockets" tmap2_complete true;
    test_in_receiving_players
      "Tmap4 change_rosters, Tatum no longer on celtics" "Jayson Tatum"
      "Boston Celtics" tmap4_complete false;
    test_in_receiving_players
      "Tmap4 change_rosters, Simons no longer on blazers"
      "Anfernee Simons" "Portland Trail Blazers" tmap4_complete false;
    test_in_remaining_players
      "Tmap4 change_rosters, Jaylen Brown still on celtics"
      "Jaylen Brown" "Boston Celtics" tmap4_complete true;
    test_in_remaining_players
      "Tmap4 change_rosters, Dame still on blazers" "Damian Lillard"
      "Portland Trail Blazers" tmap4_complete true;
    test_in_receiving_players
      "Tmap4 change_rosters, Mccollum to celtics" "C.J. McCollum"
      "Boston Celtics" tmap4_complete true;
    test_in_receiving_players
      "Tmap4 change_rosters, Smart not in receiving" "Marcus Smart"
      "Boston Celtics" tmap4_complete false;
  ]

(** [test_trademap_list_function name f team trade_map expected_output]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] with [f team trade_map]*)
let test_trademap_list_function name f team trade_map expected_output =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output
    ~printer:(pp_list pp_string) (f team trade_map)

(** [test_players_losing] constructs an OUnit test named [name] that
    asserts the equality of [expected_output] with
    [players_losing team trade_map]*)
let test_get_all_strings name trade_map expected_output =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output
    ~printer:(pp_list pp_string)
    (get_all_strings trade_map)

let get_all_strings_tests =
  [
    test_get_all_strings "tmap4 partial get all strings" tmap4_partial
      [
        "Jayson Tatum";
        "Anfernee Simons";
        "C.J. McCollum";
        "Boston Celtics";
        "Portland Trail Blazers";
      ];
    test_get_all_strings "tmap5 partial get all strings" tmap5_partial
      [
        "Oklahoma City Thunder";
        "Orlando Magic";
        "Jonathan Isaac";
        "Markelle Fultz";
        "Derrick Favors";
      ];
  ]

(** [test_in_remaining_players name player team trade_map expected_output]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] with
    [List.mem player (get_remaining_players team altered_rosters)]*)
let test_is_team_in_trade name team trade_map expected_output =
  name >:: fun _ ->
  assert_equal expected_output ~printer:string_of_bool
    (is_team_in_trade team trade_map)

let is_team_in_trade_tests =
  [
    test_is_team_in_trade "Thunder in tmap 5" "Oklahoma City Thunder"
      tmap5_complete true;
    test_is_team_in_trade "Pacers not in tmap 5" "Indiana Pacers"
      tmap5_complete false;
    test_is_team_in_trade "Pelicans in tmap2" "New Orleans Pelicans"
      tmap2_complete true;
    test_is_team_in_trade "Mavs not in tmap2" "Dallas Mavericks"
      tmap2_complete false;
    test_is_team_in_trade "Celtics not in trade after removed"
      "Boston Celtics" removed_celtics_from_trade false;
  ]

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
    test_valid_trade "invalid nonempty" invalid_tmap_ex false;
    test_valid_trade "complete valid tmap 4" tmap4_partial true;
  ]

let trademap_tests =
  List.flatten
    [
      teams_in_trade_tests;
      valid_destinations_tests;
      players_acquiring_tests;
      players_losing_tests;
      valid_trade_tests;
      get_all_strings_tests;
      change_rosters_tests;
      is_team_in_trade_tests;
      is_player_in_trade_tests;
    ]

(** [test_win_differential name team tmap expected_output] constructs an
    OUnit test named [name] that asserts the approximate equality of
    [expected_output] with [win_differential trade_map]*)
let test_win_differential name team trade_map expected_output =
  name >:: fun _ ->
  let wd = win_differential team trade_map in
  (*let _ = print_endline (string_of_float wd) in*)
  assert (float_approx_eq wd expected_output)

(** [test_ws_per_48 name player expected_output] constructs an OUnit
    test named [name] that asserts the approximate equality of
    [expected_output] with [ws_per_48 player]*)
let test_ws_per_48 name player expected_output =
  name >:: fun _ ->
  assert (float_approx_eq expected_output (ws_per_48 player))

(** [test_is_trade_viable name team tmap expected_output] constructs an
    OUnit test named [name] that asserts the approximate equality of
    [expected_output] with [is_trade_viable team tmap]*)
let test_is_trade_viable name team tmap expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (is_trade_viable team tmap)
    ~printer:string_of_bool

let is_trade_viable_tests =
  [
    test_is_trade_viable "tmap4 incomplete is viable for celtics"
      "Boston Celtics" tmap4_partial true;
    test_is_trade_viable "tmap4 incomplete is viable for blazers"
      "Portland Trail Blazers" tmap4_partial true;
    test_is_trade_viable "tmap4 complete is viable for celtics"
      "Boston Celtics" tmap4_complete true;
    test_is_trade_viable "tmap4 complete is not viable for blazers"
      "Portland Trail Blazers" tmap4_complete false;
    test_is_trade_viable "tmap5 incomplete is viable for magic"
      "Orlando Magic" tmap5_partial true;
    test_is_trade_viable "tmap5 incomplete is viable for thunder"
      "Oklahoma City Thunder" tmap5_partial true;
    test_is_trade_viable "tmap5 complete is viable for magic"
      "Orlando Magic" tmap5_complete true;
    test_is_trade_viable "tmap5 complete isn't viable for thunder"
      "Oklahoma City Thunder" tmap5_complete false;
  ]

let win_differential_tests =
  [
    test_win_differential "Rockets in map 2" "Houston Rockets"
      tmap2_complete 13.073966;
    test_win_differential "Pelicans in map 2" "New Orleans Pelicans"
      tmap2_complete (Float.neg 6.262785);
    test_win_differential "Clippers in map 2" "Los Angeles Clippers"
      tmap2_complete (Float.neg 17.2889268);
  ]

let ws_per_48_tests =
  [
    test_ws_per_48 "KZ Okpala ws per 48" "KZ Okpala" 0.010738;
    test_ws_per_48 "Kyrie ws per 48" "Kyrie Irving" 0.188335;
    test_ws_per_48 "Herbert Jones ws per 48" "Herbert Jones" 0.;
  ]

let trade_math_tests =
  List.flatten
    [ win_differential_tests; ws_per_48_tests; is_trade_viable_tests ]

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

(** [test_cap_differential name team expected_output] constructs an
    OUnit test named [name] that asserts the approximate equality of
    [expected_output] with [get_cap_differential team]*)
let test_cap_differential name team expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (get_cap_differential team)
    ~printer:string_of_int

let cap_diff_tests =
  [
    test_cap_differential "GSW cap diff" "Golden State Warriors"
      ~-39252872;
    test_cap_differential "Jazz cap diff" "Utah Jazz" ~-16442231;
    test_cap_differential "Bulls cap diff" "Chicago Bulls" 4711958;
  ]

let team_stats_tests =
  List.flatten [ win_percent_tests; cap_diff_tests ]

let suite =
  "test suite for Project"
  >::: List.flatten
         [
           trademap_tests;
           trade_math_tests;
           team_stats_tests;
           json_tests;
         ]

let _ = run_test_tt_main suite
