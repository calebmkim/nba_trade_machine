(** CS 3110 Fall 2021 Project

    @author Caleb Kim cmk265
    @author Tinsae Walelign tnw26
    @author Karan Baijal kb553
    @author Pranay Gupta pg43 *)

type work_log = {
  caleb : float;
  tinsae : float;
  karan : float;
  pranay : float;
}

val hours_worked_per_member : work_log
(** [hours_worked] is the hours worked for each team member, with the
    order of the floats corresponding the the list of names *)

val hours_worked : float
(** [hours_worked] is the total hours worked of all team members *)