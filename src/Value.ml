
type t =
    Nil
  | Bool of bool
  | Int of Num.num
  | Float of float
  | String of string
  | Array of t array
  | Dict of (t, t) Hashtbl.t
  | Function of (t list -> t)
  | Method of t * (t -> t list -> t)
  (**
   * FIXME: Ops! True definition of UserFunction is "string list * Op.t", but
   * this causes mutual recursion! For avoiding this, second type is int, which
   * indicates index of Op.user_function_ops.
   *)
  | UserFunction of string list * int

let rec string_of_value = function
    Nil -> "nil"
  | Bool b -> if b then "true" else "false"
  | Int n -> Num.string_of_num n
  | Float f -> string_of_float f
  | String s -> s
  | Array a ->
      let strings = Array.to_list (Array.map string_of_value a) in
      "[" ^ (String.concat ", " strings) ^ "]"
  | Dict h ->
      let string_of_pair key value init =
        let pair = (string_of_value key) ^ ": " ^ (string_of_value value) in
        pair :: init in
      let pairs = Hashtbl.fold string_of_pair h [] in
      if (List.length pairs = 0) then
        "{}"
      else
        "{ " ^ (String.concat ", " pairs) ^ " }"
  | Function _ -> "Function"
  | Method _ -> "Method"
  | UserFunction _ -> "UserFunction"

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
