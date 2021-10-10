open Graphics 

(**[button] represents a button with dimensions [length] by [height], lower 
left coordinate [ll] and has text [text]*)
type button = {text: string; ll: int*int; length: int; height: int} 

let build_button text ll l h = {text = text; ll = ll; length = l; height = h}

let button_clicked (b:button) x y = (x > fst b.ll) && (y > snd b.ll) && 
(x < (fst b.ll) + b.length) && (y < (snd b.ll) + b.height)

let get_button_text (b:button) = b.text 

(**[setting] measures the setting of a current screen, with 
the [identifier] being the team/player name you are showing and 
[cur_teams] being the teams involved in the trade so far*)
type setting = {identifier: string; cur_teams: string list}

let get_name_setting t = t.identifier 

let get_list_setting t = t.cur_teams  

let build_setting id team_list = {identifier= id; cur_teams = team_list}

type state = Welcome | Teams of (string list) | Roster of (setting * bool) 
|Team_transition of setting |Player of (setting * bool)
|Player_transition of string |FinalTeams of (string* (string list)) list
