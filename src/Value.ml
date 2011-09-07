
type t =
    Nil
  | Bool of bool
  | Int of Num.num
  | String of string
  | Function of (t list -> t)

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
