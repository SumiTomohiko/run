
type int_internal = Num.num

type t =
  | Nil
  | Bool of bool
  | Int of int_internal
  | Float of float
  | String of string
  | Array of t array
  | Dict of (t, t) Hashtbl.t
  | Function of (t list -> t)
  | Method of t * (t -> t list -> t)
  | UserFunction of string list * int
  | ProcessStatus of int
  | Class of string * (t -> t list -> t)
  | Exception of t * (string * string * int) list * t
  (* FIXME: Do you have better idea for iterators? *)
  | ArrayIterator of t Enum.t
  | DictIterator of (t * t) Enum.t
  | IntIterator of int_internal * int_internal

let of_num n = Int n

let to_bool = function
  | Nil -> false
  | Bool b -> b
  | ProcessStatus 0 -> true
  | ProcessStatus _ -> false
  | _ -> true

let sprintf = Printf.sprintf

let rec to_string = function
  | Nil -> "nil"
  | Bool true -> "true"
  | Bool false -> "false"
  | Int n -> Num.string_of_num n
  | Float f -> string_of_float f
  | String s -> s
  | Array a ->
      let strings = Array.to_list (Array.map to_string a) in
      sprintf "[%s]" (String.concat ", " strings)
  | Dict h ->
      let string_of_pair key value init =
        (sprintf "%s: %s" (to_string key) (to_string value)) :: init in
      let pairs = Hashtbl.fold string_of_pair h [] in
      if (List.length pairs = 0) then
        "{}"
      else
        sprintf "{ %s }" (String.concat ", " pairs)
  | Function _ -> "<Function>"
  | Method _ -> "<Method>"
  | UserFunction _ -> "<UserFunction>"
  | ProcessStatus stat -> string_of_int stat
  | Class (name, _) -> sprintf "<Class %s>" name
  | Exception (Class (name, _), _, _) -> sprintf "<%s>" name
  | _ -> assert false

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
