
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
  | JumpIfException of 'a
  | JumpIfFalse of 'a
  | Less
  | LessEqual
  | MakeArray of int
  | MakeDict of int
  | MakeException of string
  | MakeIterator
  | MakeUserFunction of string list * int
  | MoveIterator of 'a
  | MoveParam
  | Mul
  | NotEqual
  | Pop
  | PushCommand of Unix.open_flag list
  | PushCommandE2O
  | PushCommandParams of int * Param.t list
  | PushConst of Core.value
  | PushLastStatus
  | PushLocal of string
  | PushParams of int * Param.t list
  | PushPipeline of Unix.open_flag list
  | Raise
  | Reraise
  | Return
  | StoreLastException of string
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

let sprintf = Printf.sprintf

(* For debugging *)
let name_of_op = function
  | Add -> "Add"
  | Call nargs -> sprintf "Call (%d)" nargs
  | Communicate -> "Communicate"
  | Concat n -> sprintf "Concat (%d)" n
  | Div _ -> "Div"
  | DivDiv -> "DivDiv"
  | Equal -> "Equal"
  | Exec -> "Exec"
  | ExecAndPush -> "ExecAndPush"
  | GetAttr name -> sprintf "GetAttr (%s)" name
  | Greater -> "Greater"
  | GreaterEqual -> "GreaterEqual"
  | Jump dest -> sprintf "Jump (%d)" dest
  | JumpIfException dest -> sprintf "JumpIfException (%d)" dest
  | JumpIfFalse dest -> sprintf "JumpIfFalse (%d)" dest
  | Less -> "Less"
  | LessEqual -> "LessEqual"
  | MakeArray _ -> "MakeArray"
  | MakeDict _ -> "MakeDict"
  | MakeException _ -> "MakeException"
  | MakeIterator -> "MakeIterator"
  | MakeUserFunction _ -> "MakeUserFunction"
  | MoveIterator _ -> "MoveIterator"
  | MoveParam -> "MoveParam"
  | Mul -> "Mul"
  | NotEqual -> "NotEqual"
  | Pop -> "Pop"
  | PushCommand _ -> "PushCommand"
  | PushCommandE2O -> "PushCommandE2O"
  | PushCommandParams _ -> "PushCommandParams"
  | PushConst v -> sprintf "PushConst (%s)" (Core.string_of_value v)
  | PushLastStatus -> "PushLastStatus"
  | PushLocal name -> sprintf "PushLocal (%s)" name
  | PushParams _ -> "PushParams"
  | PushPipeline _ -> "PushPipeline"
  | Raise -> "Raise"
  | Reraise -> "Reraise"
  | Return -> "Return"
  | StoreLastException name -> sprintf "StoreLastException (%s)" name
  | StoreLocal _ -> "StoreLocal"
  | StoreSubscript -> "StoreSubscript"
  | Sub -> "Sub"
  | Subscript -> "Subscript"

  | Anchor -> "Anchor"
  | Label -> "Label"

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
