type t = {text: string; ll: int*int;  ur: int*int}

let is_clicked button x y = ((fst button.ll) <=x) && (fst button.ur >=x) && 
(snd button.ll <= y) && (snd button.ur >= y)


