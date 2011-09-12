
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

let eval_binop stack intf floatf =
  let right = Stack.pop stack in
  let left = Stack.pop stack in
  let result = match left, right with
      Value.Int (n), Value.Int (m) -> intf n m
    | Value.Float (x), Value.Float (y) -> floatf x y
    | _ -> raise (Failure "Unsupported operands for -") in
  Stack.push result stack

let eval_op env frame op =
  let stack = frame.stack in
  match op with
    Op.Add ->
      let intf n m = Value.Int (Num.add_num n m) in
      let floatf x y = Value.Float (x +. y) in
      eval_binop stack intf floatf
  | Op.Call (nargs) ->
      let args = pop_args frame.stack nargs in
      Stack.push (call env (Stack.pop stack) args) stack
  | Op.Div ->
      let intf n m = Value.Float (Num.float_of_num (Num.div_num n m)) in
      let floatf x y = Value.Float (x /. y) in
      eval_binop stack intf floatf
  | Op.DivDiv ->
      let intf n m = Value.Int (Num.floor_num (Num.div_num n m)) in
      let floatf x y = Value.Float (x /. y) in
      eval_binop stack intf floatf
  | Op.Exec (nargs) ->
      let string_of_value = (function
          Value.String (s) -> s
        | _ -> raise (Failure "Unsupported command")) in
      let args = List.map string_of_value (pop_args frame.stack nargs) in
      let create_process = Unix.create_process in
      let prog = List.hd args in
      let stdin = Unix.stdin in
      let stdout = Unix.stdout in
      let stderr = Unix.stderr in
      let pid = create_process prog (Array.of_list args) stdin stdout stderr in
      ignore (Unix.waitpid [] pid)
  | Op.Expand -> (* TODO *) ()
  | Op.Jump (label) -> frame.pc <- Some label
  | Op.JumpIfFalse (label) ->
      (match Stack.top stack with
        Value.Bool (false) -> ignore (Stack.pop stack); frame.pc <- Some label
      | _ -> ())
  | Op.Mul ->
      let intf n m = Value.Int (Num.mult_num n m) in
      let floatf x y = Value.Float (x *. y) in
      eval_binop stack intf floatf
  | Op.Pop -> ignore (Stack.pop stack)
  | Op.PushConst (v) -> Stack.push v stack
  | Op.PushLocal (name) -> Stack.push (find_local env frame name) stack
  | Op.StoreLocal (name) ->
      Symboltbl.add frame.locals name (Stack.pop stack)
  | Op.Sub ->
      let intf n m = Value.Int (Num.sub_num n m) in
      let floatf x y = Value.Float (x -. y) in
      eval_binop stack intf floatf
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
