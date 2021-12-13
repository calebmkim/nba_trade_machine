open Yojson.Basic.Util

let salary_info =
  Yojson.Basic.from_file "data/nbasalarydata.json" |> to_list

let get_team_info team_name =
  List.find
    (fun json_data ->
      let data = json_data |> to_assoc in
      let name = List.assoc "Name" data |> to_string in
      team_name = name)
    salary_info

let get_cap_differential team_name =
  let info = get_team_info team_name |> to_assoc in
  let diff = List.assoc "Luxury Tax Difference" info in
  diff |> to_string |> int_of_string