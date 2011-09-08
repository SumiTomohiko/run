
type binop = { left: expr; right: expr }
and call = { callee: expr; args: expr list }
and expr =
    Add of binop
  | Assign of binop
  | Const of Value.t
  | Call of call
  | Sub of binop
  | Var of string
and every = { patterns: string list; name: string; stmts: stmt list }
and stmt =
    Every of every
  | Expr of expr

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
