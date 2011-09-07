
type t = {
  locals: Symboltbl.t;
  pc: Operation.t;
  stack: Value.t Stack.t;
  prev_frame: t;
  outer_frame: t
}

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
