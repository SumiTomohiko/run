
exception Run_exception of Core.value

let rec make_traceback accum frames =
  if Stack.is_empty frames then
    accum
  else
    let frame = Stack.pop frames in
    let code = Code.code_of_index frame.Core.code in
    let path = Code.path_of_code code in
    let name = Code.name_of_code code in
    let lineno = Code.lineno_of_pc code (frame.Core.pc - 1) in
    make_traceback ((path, name, lineno) :: accum) frames

let make_exception env self args =
  let tb = make_traceback [] (Stack.copy env.Core.frames) in
  match args with
  | [] -> Core.Exception (self, tb, Core.Nil)
  | [msg] -> Core.Exception (self, tb, msg)
  | _ -> failwith "TODO: Raise ArgumentError"

let add_exception tbl name =
  Core.Symboltbl.add tbl name (Core.Class (name, make_exception))

let register_exceptions tbl =
  let exceptions = ["Exception"; "ArgumentError"; "IndexError"] in
  List.iter (add_exception tbl) exceptions

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
