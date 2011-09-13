
type t =
    Nil
  | Bool of bool
  | Int of Num.num
  | Float of float
  | String of string
  | Array of t array
  | Hash of (t, t) Hashtbl.t
  | Function of (t list -> t)

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
