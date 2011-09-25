
type t =
    Eof
  | Literal of string
  | Plain of string
  | Reference of string

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
