
type binop = { left: expr; right: expr }
and call = { callee: expr; args: expr list }
and subscript = { prefix: expr; index: expr }
and pair = { key: expr; value: expr }
and expr =
    Add of binop
  | Array of expr list
  | Assign of binop
  | Call of call
  | Const of Value.t
  | Div of binop
  | DivDiv of binop
  | Hash of pair list
  | Mul of binop
  | Sub of binop
  | Subscript of subscript
  | Var of string
and every = { patterns: string list; name: string; stmts: stmt list }
and stmt =
    Command of string list
  | Every of every
  | Expr of expr

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
