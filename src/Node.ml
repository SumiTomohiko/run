
type binop = { left: t; right: t }
and t =
    AssignExpr of binop
  | Atom of Value.t

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
