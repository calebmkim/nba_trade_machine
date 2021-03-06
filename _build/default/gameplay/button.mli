(** Button that can hold text*)

open Graphics

type t
(**The type representing a button*)

exception NoButtonClicked
(**Exception that indicates that no button was clicked*)

val get_max_size : string list -> int
(** [get_max_size lst] is the maximum size (given the current text
    settings) in pixels, of the largest button that will be produced out
    of all the strings in [lst] *)

val get_button_text : t -> string
(**[get_button_text b] is the string representing [b]'s text*)

val get_size_horz : string -> int
(**[get_size_horz s] is the horizontal size of [s] given the current
   text settings.*)

val make_button : ?max_horz:int -> string -> t
(** [make_button ?max_horz text] makes a button with text [text] and
    will move over [max_horz] pixels if there is no room to make the
    button by just moving down. [max_horz] is automatically set to the
    horizontal length of [text] if no parameter is given. *)

val make_button_list : ?max_horz:int -> string list -> t list
(** [make_button_list ?mh lst] represents a button list, and the buttons
    are printed, starting from the current x and y, going down, creating
    a column. If there is no space room to go down any more, start
    printing again from a new column which is [max_horz] pixels to the
    right of the existing button collumn. [max_horz] is the longest
    horizontal length in [lst] if parameter is not given. *)

val is_button_clicked : t -> status -> bool
(** [is_button_clicked b s] is whether [b] has been clicked or not,
    given the status [s]*)

val find_clicked_button : status -> t list -> string
(** [find_clicked_button s lst] is the string of the button [b], out of
    the [lst] of buttons given, given the status [s]. If there is no
    button clicked, then raise NoButtonClicked.*)

val make_trademap_buttons :
  (string * string list) list -> int -> (t * t list) list
(** [make_trademap_buttons tmap max_horz lst] is the button list
    corresponding to the [tmap]. [max_horz] works as usual: if there is
    no room to go down after making the button, move [max_horz] pixels
    to the right and start at the top. Each element of the list is
    composed of [(team, incoming players)] where [team] is the [team]
    button and [incoming_players] are the list of buttons that
    correspond to the incoming players for the [team]. *)
