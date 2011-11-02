
type 'a kind =
    Add
  | Call of int
  | Communicate
  | Concat of int
  | Div
  | DivDiv
  | Equal
  | Exec
  | ExecAndPush
  | GetAttr of string
  | Greater
  | GreaterEqual
  | Jump of 'a
  | JumpIfFalse of 'a
  | Less
  | LessEqual
  | MakeArray of int
  | MakeDict of int
  | MakeUserFunction of string list * int
  | MoveParam
  | Mul
  | NotEqual
  | Pop
  | PushCommand of Unix.open_flag list
  | PushCommandE2O
  | PushCommandParams of int * Param.t list
  | PushConst of Value.t
  | PushLastStatus
  | PushLocal of string
  | PushParams of int * Param.t list
  | PushPipeline of Unix.open_flag list
  | Return
  | StoreLocal of string
  | StoreSubscript
  | Sub
  | Subscript

  (* For compiler *)
  | Anchor
  | Label
and t = {
  mutable next: t option;
  kind: t kind;
  pos: Node.pos;
  mutable index: int }

let make_label pos = { next=None; kind=Label; pos=pos; index=0 }

(* For debugging *)
let name_of_op = function
    Add -> "Add"
  | Call _ -> "Call"
  | Communicate -> "Communicate"
  | Concat _ -> "Concat"
  | Div _ -> "Div"
  | DivDiv -> "DivDiv"
  | Equal -> "Equal"
  | Exec -> "Exec"
  | ExecAndPush -> "ExecAndPush"
  | GetAttr _ -> "GetAttr"
  | Greater -> "Greater"
  | GreaterEqual -> "GreaterEqual"
  | Jump _ -> "Jump"
  | JumpIfFalse _ -> "JumpIfFalse"
  | Less -> "Less"
  | LessEqual -> "LessEqual"
  | MakeArray _ -> "MakeArray"
  | MakeDict _ -> "MakeDict"
  | MakeUserFunction _ -> "MakeUserFunction"
  | MoveParam -> "MoveParam"
  | Mul -> "Mul"
  | NotEqual -> "NotEqual"
  | Pop -> "Pop"
  | PushCommand _ -> "PushCommand"
  | PushCommandE2O -> "PushCommandE2O"
  | PushCommandParams _ -> "PushCommandParams"
  | PushConst _ -> "PushConst"
  | PushLastStatus -> "PushLastStatus"
  | PushLocal _ -> "PushLocal"
  | PushParams _ -> "PushParams"
  | PushPipeline _ -> "PushPipeline"
  | Return -> "Return"
  | StoreLocal _ -> "StoreLocal"
  | StoreSubscript -> "StoreSubscript"
  | Sub -> "Sub"
  | Subscript -> "Subscript"

  | Anchor -> "Anchor"
  | Label -> "Label"

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
