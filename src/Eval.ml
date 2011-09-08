
type frame = {
  locals: Symboltbl.t;
  pc: Operation.t list ref;
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
  | n -> (pop_args stack (n - 1)) @ [Stack.pop stack]

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

let eval_op env frame op =
  let stack = frame.stack in
  match op with
    Operation.Add ->
      let right = Stack.pop stack in
      let left = Stack.pop stack in
      (match left, right with
        Value.Int (n1), Value.Int (n2) ->
          Stack.push (Value.Int (Num.add_num n1 n2)) stack
      | _ -> raise (Failure "Unsupported operands for +"))
  | Operation.Call (nargs) ->
      let args = pop_args frame.stack nargs in
      Stack.push (call env (Stack.pop stack) args) stack
  | Operation.Pop -> ignore (Stack.pop stack)
  | Operation.PushConst (v) -> Stack.push v stack
  | Operation.PushLocal (name) -> Stack.push (find_local env frame name) stack
  | Operation.StoreLocal (name) ->
      Symboltbl.add frame.locals name (Stack.pop stack)
  | Operation.Sub ->
      let right = Stack.pop stack in
      let left = Stack.pop stack in
      (match left, right with
        Value.Int (n1), Value.Int (n2) ->
          Stack.push (Value.Int (Num.sub_num n1 n2)) stack
      | _ -> raise (Failure "Unsupported operands for -"))

let rec eval_env env =
  let frame = Stack.top env.frames in
  let pc = !(frame.pc) in
  if List.length pc = 0 then
    ()
  else
    let op = List.hd pc in
    frame.pc := List.tl pc;
    eval_op env frame op;
    eval_env env

let eval ops =
  let frame = {
    locals=Symboltbl.create ();
    pc=ref ops;
    stack=Stack.create ();
    prev_frame=None;
    outer_frame=None } in
  let stack = Stack.create () in
  Stack.push frame stack;
  eval_env { globals=Builtins.create (); frames=stack }

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
