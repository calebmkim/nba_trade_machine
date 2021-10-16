open Graphics

type t
(**The type representing a button*)

val get_max_size : string list -> int
(** [get_max_size lst] is the maximum size (given the current text
    settings) in pixels, of the largest button that will be produced out
    of all the strings in [lst] *)

val get_button_text : t -> string
(*[get_button_text b] is the string representing [b]'s text*)

val get_size_horz : string -> int
(**[get_size_horz] is the horizontal size of the string given the
   current text settings.*)

val print_name : string -> int -> int -> int -> t
(** [print_name s x y m] is the button that has text [s] written in it
    and whose lower left coordinates are [(x,y)]. If the button will go
    off the bottom of the page, then places the button at the top of the
    page, but moved [m] pixels horizontally. If not, moves the current
    (x,y) such that the coordinates are
    [(x, y- vertical size of button)] *)

val make_button_list : string list -> t list
(** [make_button_list lst] represents a button list, and the buttons are
    printed, starting from the current x and y, going down, creating a
    column. If there is no space left, start printing again from a new
    column, adjacent and to the right of the existing button collumn. *)

val button_clicked : t -> int -> int -> bool
(** [button_clicked t x y] is whether [t] has been clicked or not, given
    the x and y coordinates of the click*)

val find_clicked_button : status -> t list -> string
(** [find_clicked_button s lst] is the string of the button [b], out of
    the [lst] of buttons given, given the status [s]. If there is no
    button clicked, then raise Failure "No button clicked".*)
