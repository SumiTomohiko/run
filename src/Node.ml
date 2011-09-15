
type binop = { left: expr; right: expr }
and call = { callee: expr; args: expr list }
and subscript = { prefix: expr; index: expr }
and pair = { key: expr; value: expr }
and attr = { attr_prefix: expr; attr_name: string }
and expr =
    Add of binop
  | Array of expr list
  | Assign of binop
  | Attr of attr
  | Call of call
  | Const of Value.t
  | Dict of pair list
  | Div of binop
  | DivDiv of binop
  | EqualEqual of binop
  | Greater of binop
  | GreaterEqual of binop
  | Heredoc of Buffer.t
  | Less of binop
  | LessEqual of binop
  | Mul of binop
  | NotEqual of binop
  | Sub of binop
  | Subscript of subscript
  | Var of string
and every = { patterns: string list; names: string list; stmts: stmt list }
and user_function = {
  uf_name: string;
  uf_args: string list;
  uf_stmts: stmt list
}
and stmt =
    Break
  | Command of string list
  | Every of every
  | Expr of expr
  | If of expr * stmt list * stmt list
  | Next
  | Return of expr
  | UserFunction of user_function
  | While of expr * stmt list

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
