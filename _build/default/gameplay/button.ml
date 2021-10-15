type button = {
  text : string;
  ll : int * int;
  length : int;
  height : int;
}
(**[button] represents a button with dimensions [length] by [height],
   lower left coordinate [ll] and has text [text]*)

let build_button text ll l h = { text; ll; length = l; height = h }

let button_clicked (b : button) x y =
  x > fst b.ll
  && y > snd b.ll
  && x < fst b.ll + b.length
  && y < snd b.ll + b.height

let get_button_text (b : button) = b.text