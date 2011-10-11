
let output f args =
  List.iter (fun v -> print_string (f v)) args;
  Value.Nil

let builtins = [
  ("print", output Value.string_of_value);
  ("puts", output (fun v -> (Value.string_of_value v) ^ "\n"))]

let create () =
  let tbl = Symboltbl.create () in
  let f = fun init (name, f) -> Symboltbl.add tbl name (Value.Function f) in
  List.fold_left f () builtins;
  tbl

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
