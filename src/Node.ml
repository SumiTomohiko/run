
type binop = { left: expr; right: expr }
and call = { callee: expr; args: expr list }
and expr =
    Add of binop
  | Array of expr list
  | Assign of binop
  | Call of call
  | Const of Value.t
  | Div of binop
  | DivDiv of binop
  | Mul of binop
  | Sub of binop
  | Var of string
and every = { patterns: string list; name: string; stmts: stmt list }
and stmt =
    Command of string list
  | Every of every
  | Expr of expr

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
