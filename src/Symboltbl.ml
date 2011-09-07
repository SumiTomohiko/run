
type t = (string, Value.t) Hashtbl.t

let create () = Hashtbl.create 0
let find = Hashtbl.find
let add = Hashtbl.add

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
