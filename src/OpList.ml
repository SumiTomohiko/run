
type t = { top: Op.t; mutable last: Op.t }

let make () =
  let anchor = {
    Op.next=None;
    Op.kind=Op.Anchor;
    Op.pos=Node.dummy_pos;
    Op.index=0 } in
  { top=anchor; last=anchor }

let add oplist op =
  oplist.last.Op.next <- Some op;
  oplist.last <- op

let compute_next_index oplist =
  let last = oplist.last in
  last.Op.index + match last.Op.kind with
  | Op.Anchor
  | Op.Label -> 0
  | _ -> 1

let add_label oplist label =
  label.Op.index <- compute_next_index oplist;
  add oplist label

let add_op oplist kind pos =
  let index = compute_next_index oplist in
  add oplist { Op.next=None; Op.kind=kind; Op.pos=pos; Op.index=index }

let top oplist = oplist.top

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
