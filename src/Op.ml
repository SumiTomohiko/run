
type kind =
    Add
  | Call of int
  | Div
  | DivDiv
  | Equal
  | Exec of int
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
  | Mul
  | NotEqual
  | Pop
  | PushConst of Value.t
  | PushLocal of string
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
  | Call (_) -> "Call"
  | Div (_) -> "Div"
  | DivDiv -> "DivDiv"
  | Equal -> "Equal"
  | Exec (_) -> "Exec"
  | Expand -> "Expand"
  | GetAttr _ -> "GetAttr"
  | Greater -> "Greater"
  | GreaterEqual -> "GreaterEqual"
  | Jump (_) -> "Jump"
  | JumpIfFalse (_) -> "JumpIfFalse"
  | Less -> "Less"
  | LessEqual -> "LessEqual"
  | MakeArray _ -> "MakeArray"
  | MakeDict _ -> "MakeDict"
  | MakeUserFunction _ -> "MakeUserFunction"
  | Mul -> "Mul"
  | NotEqual -> "NotEqual"
  | Pop -> "Pop"
  | PushConst (_) -> "PushConst"
  | PushLocal (_) -> "PushLocal"
  | Return -> "Return"
  | StoreLocal (_) -> "StoreLocal"
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
