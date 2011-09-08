
type kind =
    Add
  | Call of int
  | Expand
  | Jump of t
  | JumpIfFalse of t
  | Pop
  | PushConst of Value.t
  | PushLocal of string
  | StoreLocal of string
  | Sub

  (* For compiler *)
  | Anchor
  | Label
and t = { mutable next: t option; kind: kind }

let make kind = { next=None; kind=kind }
let make_label () = make Label
let next_of_op op = op.next
let kind_of_op op = op.kind

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
