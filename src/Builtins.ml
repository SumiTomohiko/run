
module Command = struct
  type t = (string, string list -> int * string) Hashtbl.t

  let exit = function
    | [] -> exit 0
    | [stat] -> exit (int_of_string stat)
    (* TODO: raise ArgumentError *)
    | _ -> failwith "exit accepts one optional parameter."

  let cd = function
    | [dir] ->
        Unix.chdir dir;
        0, ""
    (* TODO: raise ArgumentError *)
    | _ -> failwith "cd accepts only one parameter."

  let find = Hashtbl.find

  let create () =
    let hash = Hashtbl.create 31 in
    List.iter (fun (name, f) -> Hashtbl.add hash name f) [
      ("cd", cd);
      ("exit", exit)];
    hash
end

module Function = struct
  let output f args =
    List.iter (fun v -> print_string (f v)) args;
    Value.Nil

  let builtins = [
    ("print", output Value.to_string);
    ("puts", output (fun v -> (Value.to_string v) ^ "\n"))]

  let create () =
    let tbl = Symboltbl.create () in
    let add (name, f) = Symboltbl.add tbl name (Value.Function f) in
    List.iter add builtins;
    tbl
end

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
