
type kind =
    Add
  | Call of int
  | Div
  | DivDiv
  | Exec of int
  | Expand
  | GetAttr of string
  | Jump of t
  | JumpIfFalse of t
  | MakeArray of int
  | MakeDict of int
  | MakeUserFunction of string list * int
  | Mul
  | Pop
  | PushConst of Value.t
  | PushLocal of string
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
  | Exec (_) -> "Exec"
  | Expand -> "Expand"
  | GetAttr _ -> "GetAttr"
  | Jump (_) -> "Jump"
  | JumpIfFalse (_) -> "JumpIfFalse"
  | MakeArray _ -> "MakeArray"
  | MakeDict _ -> "MakeDict"
  | MakeUserFunction _ -> "MakeUserFunction"
  | Mul -> "Mul"
  | Pop -> "Pop"
  | PushConst (_) -> "PushConst"
  | PushLocal (_) -> "PushLocal"
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
