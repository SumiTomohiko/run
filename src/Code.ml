
type 'a entry = int * int * 'a
type t = string * string * int entry array * int Op.kind array

let rec find tbl target index not_found =
  if (Array.length tbl) = index then
    not_found
  else
    let front, rear, datum = Array.get tbl index in
    if (front <= target) && (target < rear) then
      datum
    else
      find tbl target (index + 1) not_found

let ops_of_code (_, _, _, ops) = ops
let path_of_code (path, _, _, _) = path
let name_of_code (_, name, _, _) = name
let lineno_of_pc (_, _, tbl, _) pc = find tbl pc 0 (-1)

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
