
type frame = {
  locals: Symboltbl.t;
  mutable pc: Op.t option;
  stack: Value.t Stack.t;
  prev_frame: frame option;
  outer_frame: frame option
}

type env = {
  globals: Symboltbl.t;
  frames: frame Stack.t
}

let rec pop_args stack = function
    0 -> []
  | n -> let last = Stack.pop stack in (pop_args stack (n - 1)) @ [last]

let call env callee args =
  match callee with
    Value.Function (f) -> f args
  | _ -> raise (Failure "Object is not callable")

let find_global env = Symboltbl.find env.globals

let find_local env frame name =
  try
    Symboltbl.find frame.locals name
  with
    Not_found -> find_global env name

let raise_unsupported_operands_error () =
  raise (Failure "Unsupported operands")

let eval_binop stack intf floatf stringf string_intf =
  let right = Stack.pop stack in
  let left = Stack.pop stack in
  let result = match left, right with
      Value.Int (n), Value.Int (m) -> intf n m
    | Value.Float (x), Value.Float (y) -> floatf x y
    | Value.String (s), Value.String (t) -> stringf s t
    | Value.Int (n), Value.String (s) -> string_intf s n ""
    | Value.String (s), Value.Int (n) -> string_intf s n ""
    | _ -> raise_unsupported_operands_error () in
  Stack.push result stack

let get_array_attr a = function
    "size" -> Value.Int (Num.num_of_int (Array.length a))
  | name -> raise (Failure ("AttributeError: " ^ name))

let eval_op env frame op =
  let stack = frame.stack in
  let error _ = raise_unsupported_operands_error () in
  match op with
    Op.Add ->
      let intf n m = Value.Int (Num.add_num n m) in
      let floatf x y = Value.Float (x +. y) in
      let stringf s t = Value.String (s ^ t) in
      eval_binop stack intf floatf stringf error
  | Op.Call (nargs) ->
      let args = pop_args frame.stack nargs in
      Stack.push (call env (Stack.pop stack) args) stack
  | Op.Div ->
      let intf n m = Value.Float (Num.float_of_num (Num.div_num n m)) in
      let floatf x y = Value.Float (x /. y) in
      eval_binop stack intf floatf error error
  | Op.DivDiv ->
      let intf n m = Value.Int (Num.floor_num (Num.div_num n m)) in
      let floatf x y = Value.Float (x /. y) in
      eval_binop stack intf floatf error error
  | Op.Exec (nargs) ->
      let string_of_value = (function
          Value.String (s) -> s
        | _ -> raise (Failure "Unsupported command")) in
      let args = List.map string_of_value (pop_args stack nargs) in
      let create_process = Unix.create_process in
      let prog = List.hd args in
      let stdin = Unix.stdin in
      let stdout = Unix.stdout in
      let stderr = Unix.stderr in
      let pid = create_process prog (Array.of_list args) stdin stdout stderr in
      ignore (Unix.waitpid [] pid)
  | Op.Expand -> (* TODO *) ()
  | Op.GetAttr name ->
      let attr = match Stack.pop stack with
        Value.Array (a) -> get_array_attr a name
      | _ -> raise (Failure "Unknown object") in
      Stack.push attr stack
  | Op.Jump (label) -> frame.pc <- Some label
  | Op.JumpIfFalse (label) ->
      (match Stack.top stack with
        Value.Bool (false) -> ignore (Stack.pop stack); frame.pc <- Some label
      | _ -> ())
  | Op.MakeArray size ->
      Stack.push (Value.Array (Array.of_list (pop_args stack size))) stack
  | Op.MakeDict size ->
      let hash = Hashtbl.create 0 in
      let rec iter n =
        if n = 0 then
          ()
        else
          let value = Stack.pop stack in
          let key = Stack.pop stack in
          Hashtbl.replace hash key value;
          iter (n - 1) in
      iter size;
      Stack.push (Value.Dict hash) stack
  | Op.Mul ->
      let intf n m = Value.Int (Num.mult_num n m) in
      let floatf x y = Value.Float (x *. y) in
      let rec string_intf s n accum =
        if Num.eq_num n (Num.num_of_int 0) then
          Value.String accum
        else
          string_intf s (Num.pred_num n) (accum ^ s) in
      eval_binop stack intf floatf error string_intf
  | Op.Pop -> ignore (Stack.pop stack)
  | Op.PushConst (v) -> Stack.push v stack
  | Op.PushLocal (name) -> Stack.push (find_local env frame name) stack
  | Op.StoreLocal (name) ->
      Symboltbl.add frame.locals name (Stack.pop stack)
  | Op.StoreSubscript ->
      let index = Stack.pop stack in
      let prefix = Stack.pop stack in
      let value = Stack.top stack in
      (match prefix, index with
        Value.Array (a), Value.Int (Num.Int (n)) -> Array.set a n value
      | Value.Dict h, _ -> Hashtbl.replace h index value
      | _ -> raise (Failure "Invalid subscript operation"))
  | Op.Sub ->
      let intf n m = Value.Int (Num.sub_num n m) in
      let floatf x y = Value.Float (x -. y) in
      eval_binop stack intf floatf error error
  | Op.Subscript ->
      let index = Stack.pop stack in
      let prefix = Stack.pop stack in
      let value = match prefix, index with
        Value.Array (a), Value.Int (Num.Int (n)) -> Array.get a n
      | Value.Dict h, _ -> Hashtbl.find h index
      | _ -> raise (Failure "Invalid subscript operation") in
      Stack.push value stack

  | Op.Anchor -> ()
  | Op.Label -> ()

let rec eval_env env =
  let frame = Stack.top env.frames in
  match frame.pc with
    Some (op) ->
      (frame.pc <- Op.next_of_op op;
      eval_op env frame (Op.kind_of_op op);
      eval_env env)
  | None -> ()

let eval ops =
  let frame = {
    locals=Symboltbl.create ();
    pc=Some ops;
    stack=Stack.create ();
    prev_frame=None;
    outer_frame=None } in
  let stack = Stack.create () in
  Stack.push frame stack;
  eval_env { globals=Builtins.create (); frames=stack }

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
