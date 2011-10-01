
type t =
    Branch of t list list
  | Char of char
  | Star
  | StarStar

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
