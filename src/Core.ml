
type command = {
  cmd_params: string DynArray.t;
  cmd_stderr_redirect: Redirect.t option }

type pipeline = {
  pl_commands: command DynArray.t;
  pl_first_stdin: string option;
  pl_last_stdout: (string * Unix.open_flag list) option }

type int_internal = Num.num

type symboltbl = (string, value) Hashtbl.t
and frame = {
  code: int;
  mutable pc: int;
  locals: symboltbl;
  outer_frame: frame option;
  stack: value Stack.t;
  pipelines: pipeline Stack.t }
and env = {
  globals: symboltbl;
  builtins: (string, env -> string list -> (int * string)) Hashtbl.t;
  frames: frame Stack.t;
  mutable last_status: int;
  mutable last_exception: value }
and value =
  | Nil
  | Bool of bool
  | Int of int_internal
  | Float of float
  | String of string
  | Array of value array
  | Dict of (value, value) Hashtbl.t
  | Function of (value list -> value)
  | Method of value * (env -> value -> value list -> value)
  | UserFunction of string list * int
  | ProcessStatus of int
  | Class of string * (env -> value -> value list -> value)
  | Exception of value * (string * string * int) list * value
  (* FIXME: Do you have better idea for iterators? *)
  | ArrayIterator of value Enum.t
  | DictIterator of (value * value) Enum.t
  | IntIterator of int_internal * int_internal

let value_of_num n = Int n
let value_of_string s = String s
let value_of_bool b = Bool b
let value_of_float f = Float f

let bool_of_value = function
  | Nil -> false
  | Bool b -> b
  | ProcessStatus 0 -> true
  | ProcessStatus _ -> false
  | _ -> true

let sprintf = Printf.sprintf

let rec string_of_value = function
  | Nil -> "nil"
  | Bool true -> "true"
  | Bool false -> "false"
  | Int n -> Num.string_of_num n
  | Float f -> string_of_float f
  | String s -> s
  | Array a ->
      let strings = Array.to_list (Array.map string_of_value a) in
      sprintf "[%s]" (String.concat ", " strings)
  | Dict h ->
      let string_of_pair key value init =
        let s = string_of_value key in
        let t = string_of_value value in
        (sprintf "%s: %s" s t) :: init in
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

let make_frame code locals = {
  code=code;
  pc=0;
  locals=locals;
  outer_frame=None;
  stack=Stack.create ();
  pipelines=Stack.create () }

module Symboltbl = struct
  let create () = Hashtbl.create 0
  let find = Hashtbl.find
  let add = Hashtbl.add
end

let create_env index globals builtins =
  let frame = make_frame index (Symboltbl.create ()) in
  let frames = Stack.create () in
  Stack.push frame frames;
  {
    globals=globals;
    builtins=builtins;
    frames=frames;
    last_status=0;
    last_exception=Nil }

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
