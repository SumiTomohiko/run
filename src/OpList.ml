
type t = { top: Op.t; mutable last: Op.t }

let make () =
  let anchor = { Op.next=None; Op.kind=Op.Anchor } in
  { top=anchor; last=anchor }

let add_op oplist op =
  oplist.last.Op.next <- Some op;
  oplist.last <- op

let add oplist kind = add_op oplist { Op.next=None; Op.kind=kind }

let top oplist = oplist.top

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
