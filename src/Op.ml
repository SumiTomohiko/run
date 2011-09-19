
type kind =
    Add
  | Call of int
  | DefineRedirectOut
  | Div
  | DivDiv
  | Equal
  | Exec
  | Expand
  | GetAttr of string
  | Greater
  | GreaterEqual
  | Jump of t
  | JumpIfFalse of t
  | Less
  | LessEqual
  | MakeArray of int
  | MakeDict of int
  | MakeUserFunction of string list * int
  | MoveParam
  | Mul
  | NotEqual
  | Pop
  | PushCommand
  | PushConst of Value.t
  | PushLocal of string
  | PushPipeline
  | Return
  | StoreLocal of string
  | StoreSubscript
  | Sub
  | Subscript

  (* For compiler *)
  | Anchor
  | Label
and t = { mutable next: t option; kind: kind }

let make kind = { next=None; kind=kind }
let make_label () = make Label
let next_of_op op = op.next
let kind_of_op op = op.kind
let name_of_op = function
    Add -> "Add"
  | Call _ -> "Call"
  | DefineRedirectOut -> "DefineRedirectOut"
  | Div _ -> "Div"
  | DivDiv -> "DivDiv"
  | Equal -> "Equal"
  | Exec -> "Exec"
  | Expand -> "Expand"
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
  | PushCommand -> "PushCommand"
  | PushConst _ -> "PushConst"
  | PushLocal _ -> "PushLocal"
  | PushPipeline -> "PushPipeline"
  | Return -> "Return"
  | StoreLocal _ -> "StoreLocal"
  | StoreSubscript -> "StoreSubscript"
  | Sub -> "Sub"
  | Subscript -> "Subscript"

  | Anchor -> "Anchor"
  | Label -> "Label"

let ops: t list ref = ref []

let register_ops op =
  ops := !ops @ [op];
  (List.length (!ops)) - 1

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
