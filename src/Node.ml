
type binop = { left: expr; right: expr }
and call = { callee: expr; args: expr list }
and expr =
    Add of binop
  | Assign of binop
  | Const of Value.t
  | Call of call
  | Var of string
and stmt = Expr of expr

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
