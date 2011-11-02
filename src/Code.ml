
type 'a table = int * int * 'a
type t = string * int table array * int Op.kind array

let ops_of_code (_, _, ops) = ops

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
