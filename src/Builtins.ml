
let rec string_of_value = function
    Value.Nil -> "nil"
  | Value.Bool (b) -> if b then "true" else "false"
  | Value.Int (n) -> Num.string_of_num n
  | Value.Float (f) -> string_of_float f
  | Value.String (s) -> s
  | Value.Array a ->
      let strings = Array.to_list (Array.map string_of_value a) in
      "[" ^ (String.concat ", " strings) ^ "]"
  | Value.Hash h ->
      let string_of_pair key value init =
        let pair = (string_of_value key) ^ ": " ^ (string_of_value value) in
        pair :: init in
      let pairs = Hashtbl.fold string_of_pair h [] in
      if (List.length pairs = 0) then
        "{}"
      else
        "{ " ^ (String.concat ", " pairs) ^ " }"
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
