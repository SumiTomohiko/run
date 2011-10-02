
type kind =
    Add
  | Call of int
  | Communicate
  | Div
  | DivDiv
  | Equal
  | Exec
  | ExecAndPush
  | Expand of Matching.Main.pattern
  | ExpandParam of Matching.Main.pattern
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
  | PushCommand of Unix.open_flag list
  | PushCommandE2O
  | PushConst of Value.t
  | PushLastStatus
  | PushLocal of string
  | PushPipeline of Unix.open_flag list
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
  | Communicate -> "Communicate"
  | Div _ -> "Div"
  | DivDiv -> "DivDiv"
  | Equal -> "Equal"
  | Exec -> "Exec"
  | ExecAndPush -> "ExecAndPush"
  | Expand _ -> "Expand"
  | ExpandParam _ -> "ExpandParam"
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
  | PushConst _ -> "PushConst"
  | PushLastStatus -> "PushLastStatus"
  | PushLocal _ -> "PushLocal"
  | PushPipeline _ -> "PushPipeline"
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
