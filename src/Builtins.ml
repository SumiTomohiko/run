
let string_of_value = function
    Value.Nil -> "nil"
  | Value.Bool (b) -> if b then "true" else "false"
  | Value.Int (n) -> Num.string_of_num n
  | Value.String (s) -> s
  | Value.Function (_) -> "Function"

let print_value v = print_string (string_of_value v)

let print args =
  List.fold_left (fun init v -> print_value v; init) Value.Nil args

let builtins = [("print", print)]

let create () =
  let tbl = Symboltbl.create () in
  let f = fun init (name, f) -> Symboltbl.add tbl name (Value.Function f) in
  List.fold_left f () builtins;
  tbl

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
