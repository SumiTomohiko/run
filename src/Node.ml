
type pos = string * int

type binop = { left: expr; right: expr }
and call = { callee: expr; args: expr list }
and subscript = { prefix: expr; index: expr }
and pair = { key: expr; value: expr }
and attr = { attr_prefix: expr; attr_name: string }
and expr_body =
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
  | InlinePipeline of command list
  | LastStatus
  | Less of binop
  | LessEqual of binop
  | Mul of binop
  | NotEqual of binop
  | String of expr list
  | Sub of binop
  | Subscript of subscript
  | Var of string
and expr = pos * expr_body
and every = { params: param list; names: string list; stmts: stmt list }
and user_function = {
  uf_name: string;
  uf_args: string list;
  uf_stmts: stmt list }
and redirect = Dup | File of string * Unix.open_flag list
and param = param_atom list
and expr_param = { mutable ep_index: int; ep_expr: expr }
and param_atom =
  | Branch of param list
  | Char of char
  | Dir
  | ExprParam of expr_param
  | Star
  | StarStar
and command = param list * string option * redirect option * redirect option
and except = expr list * string option * stmt list
and stmt_body =
    Break
  | Communication of param list * param list
  | Every of every
  | Expr of expr
  | If of expr * stmt list * stmt list
  | Next
  | Pipeline of command list
  | Raise of expr option
  | Return of expr
  | Try of stmt list * except list * stmt list
  | UserFunction of user_function
  | While of expr * stmt list

  (* For an empty line *)
  | Empty
and stmt = pos * stmt_body

let dummy_pos = ("", -1)

let rec dump = function
  | hd :: tl ->
      let name = match hd with
      | Break -> "Break"
      | Communication _ -> "Communication"
      | Every _ -> "Every"
      | Expr _ -> "Expr"
      | If _ -> "If"
      | Next -> "Next"
      | Pipeline _ -> "Pipeline"
      | Raise _ -> "Raise"
      | Return _ -> "Return"
      | Try _ -> "Try"
      | UserFunction _ -> "UserFunction"
      | While _ -> "While"
      | Empty -> "Empty" in
      Printf.printf "%s\n" name;
      dump tl
  | [] -> ()

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
