
type t =
    Add
  | Call of int
  | Pop
  | PushConst of Value.t
  | PushLocal of string

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
