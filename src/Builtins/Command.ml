
type t = (string, string list -> int * string) Hashtbl.t

let exit = function
  | [] -> exit 0
  | [stat] -> exit (int_of_string stat)
  (* TODO: raise ArgumentError *)
  | _ -> failwith "exit accept one positional parameter."

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

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
