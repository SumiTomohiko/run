
let string_of_value = function
    Value.Nil -> "nil"
  | Value.Bool (b) -> if b then "true" else "false"
  | Value.Int (n) -> Num.string_of_num n
  | Value.Float (f) -> string_of_float f
  | Value.String (s) -> s
  | Value.Function (_) -> "Function"

let output args f =
  List.fold_left (fun init v -> print_string (f v); init) Value.Nil args

let print args = output args string_of_value

let puts args = output args (fun v -> (string_of_value v) ^ "\n")

let builtins = [
  ("print", print);
  ("puts", puts)]

let create () =
  let tbl = Symboltbl.create () in
  let f = fun init (name, f) -> Symboltbl.add tbl name (Value.Function f) in
  List.fold_left f () builtins;
  tbl

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
