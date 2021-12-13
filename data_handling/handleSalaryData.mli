(** Functions to get information about team's salary cap room *)

val get_cap_differential : string -> int
(**[get_cap_differential n] is the cap differential (i.e the amount of
   cap room) of team with name [n]. A negative number indicates that
   they are over the luxary tax line. Read more on NBA salary cap rules
   (https://en.wikipedia.org/wiki/NBA_salary_cap or
   https://cbabreakdown.com/salary-cap-overview) if this terminology is
   unclear. Requires: [n] is the name of a valid NBA team*)
