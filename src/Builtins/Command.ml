
type t = (string, string list -> int * string) Hashtbl.t

let cd = function
  | [dir] ->
      Unix.chdir dir;
      0, ""
  (* TODO: raise ArgumentError *)
  | _ -> failwith "cd accepts only one parameter."

let find = Hashtbl.find

let create () =
  let hash = Hashtbl.create 31 in
  Hashtbl.add hash "cd" cd;
  hash

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
