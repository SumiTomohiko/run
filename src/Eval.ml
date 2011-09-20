
type command = {
  cmd_params: string DynArray.t;
  mutable cmd_path: string option
}

type frame = {
  mutable pc: Op.t option;
  locals: Symboltbl.t;
  outer_frame: frame option;
  stack: Value.t Stack.t;
  pipelines: command list Stack.t
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
    Value.Function f -> f args
  | Value.Method (self, f) -> f self args
  | _ -> failwith "Object is not callable"

let find_global env = Symboltbl.find env.globals

let find_local env frame name =
  try
    Symboltbl.find frame.locals name
  with
    Not_found -> find_global env name

let raise_unsupported_operands_error () =
  failwith "Unsupported operands"

let eval_binop stack intf floatf stringf string_intf =
  let right = Stack.pop stack in
  let left = Stack.pop stack in
  let result = match left, right with
      Value.Int n, Value.Int m -> intf n m
    | Value.Float x, Value.Float y -> floatf x y
    | Value.String s, Value.String t -> stringf s t
    | Value.Int n, Value.String s -> string_intf s n ""
    | Value.String s, Value.Int n -> string_intf s n ""
    | _ -> raise_unsupported_operands_error () in
  Stack.push result stack

let array_expand self _ =
  match self with
    Value.Array a ->
      let strings = Array.to_list (Array.map Builtins.string_of_value a) in
      Value.String (String.concat " " strings)
  | _ -> failwith "self must be Array"

let dict_expand self _ =
  match self with
    Value.Dict h ->
      let f key value init =
        let s = Builtins.string_of_value key in
        let t = Builtins.string_of_value value in
        (s ^ " " ^ t) :: init in
      Value.String (String.concat " " (Hashtbl.fold f h []))
  | _ -> failwith "self must be Dict"

let get_dict_attr h = function
    "expand" -> Value.Method ((Value.Dict h), dict_expand)
  | name -> failwith ("AttributeError: " ^ name)

let get_array_attr a = function
    "size" -> Value.Int (Num.num_of_int (Array.length a))
  | "expand" -> Value.Method (Value.Array a, array_expand)
  | name -> failwith ("AttributeError: " ^ name)

let eval_comparison stack f =
  let right = Stack.pop stack in
  let left = Stack.pop stack in
  let result = match left, right with
    Value.Int n, Value.Int m -> compare n m
  | Value.Float f, Value.Float g -> compare f g
  | Value.String s, Value.String t -> compare s t
  | _ -> failwith "Invalid comparison" in
  Stack.push (Value.Bool (f result)) stack

let eval_equality stack f =
  let right = Stack.pop stack in
  let left = Stack.pop stack in
  let result = match left, right with
    Value.Int n, Value.Int m -> compare n m
  | Value.Float f, Value.Float g -> compare f g
  | Value.String s, Value.String t -> compare s t
  | _ -> 1 in
  Stack.push (Value.Bool (f result)) stack

let rec make_pipes pairs prev_pair last_pair = function
    [hd] -> pairs @ [(prev_pair, last_pair)]
  | hd :: tl ->
    let rfd, wfd = Unix.pipe () in
    let pair = (Some rfd, Some wfd) in
    make_pipes (pairs @ [(prev_pair, pair)]) pair last_pair tl
  (* OCaml 3.12.0 cannot detect that next pattern is never used *)
  | [] -> assert false

let dup oldfd newfd = Unix.dup2 (Option.default newfd oldfd) newfd

let exec_cmd cmd (pipe1, pipe2) =
  match Unix.fork () with
    0 ->
      (match snd pipe1 with
        Some fd -> Unix.close fd
      | None -> ());
      (match fst pipe2 with
        Some fd -> Unix.close fd
      | None -> ());
      let args = cmd.cmd_params in
      let prog = DynArray.get args 0 in
      dup (fst pipe1) Unix.stdin;
      dup (snd pipe2) Unix.stdout;
      Unix.execvp prog (DynArray.to_array args)
  | pid -> pid

let close = Option.may Unix.close

let rec close_pipes = function
    (_, pair) :: tl ->
      close (fst pair);
      close (snd pair);
      close_pipes tl
  | [] -> ()

let eval_op env frame op =
  let stack = frame.stack in
  let error _ = raise_unsupported_operands_error () in
  match op with
    Op.Add ->
      let intf n m = Value.Int (Num.add_num n m) in
      let floatf x y = Value.Float (x +. y) in
      let stringf s t = Value.String (s ^ t) in
      eval_binop stack intf floatf stringf error
  | Op.Call nargs ->
      let args = pop_args frame.stack nargs in
      let f = Stack.pop stack in
      (match f with
        Value.UserFunction (params, index) ->
          let locals = Symboltbl.create () in
          let store_param param arg = Symboltbl.add locals param arg in
          List.iter2 store_param params args;
          let ops = List.nth !Op.ops index in
          let frame = {
            pc=Some ops;
            locals=locals;
            outer_frame=None;
            stack=Stack.create ();
            pipelines=Stack.create () } in
          Stack.push frame env.frames
      | _ -> Stack.push (call env f args) stack)
  | Op.DefineRedirectOut ->
      let cmd = List.hd (Stack.top frame.pipelines) in
      (match Stack.pop stack with
        Value.String path -> cmd.cmd_path <- Some path
      | _ -> failwith "Unsupported redirection")
  | Op.Div ->
      let intf n m = Value.Float (Num.float_of_num (Num.div_num n m)) in
      let floatf x y = Value.Float (x /. y) in
      eval_binop stack intf floatf error error
  | Op.DivDiv ->
      let intf n m = Value.Int (Num.floor_num (Num.div_num n m)) in
      let floatf x y = Value.Float (x /. y) in
      eval_binop stack intf floatf error error
  | Op.Equal -> eval_equality stack ((=) 0)
  | Op.Exec ->
      let pipeline = Stack.pop frame.pipelines in
      let first_pair = ((match List.hd pipeline with
        { cmd_params; cmd_path=Some path } ->
          Some (Unix.openfile path [Unix.O_RDONLY] 0)
      | _ -> None), None) in
      let last_pair = (None, match List.hd (List.rev pipeline) with
        { cmd_params; cmd_path=Some path } ->
          let flag = [Unix.O_WRONLY; Unix.O_CREAT] in
          Some (Unix.openfile path flag 0o644)
      | _ -> None) in
      let pipes = make_pipes [] first_pair last_pair pipeline in
      let pids = List.map2 exec_cmd (List.rev pipeline) pipes in
      close_pipes pipes;
      List.iter (fun pid -> ignore (Unix.waitpid [] pid)) pids
  | Op.Expand -> (* TODO *) ()
  | Op.Greater -> eval_comparison stack ((<) 0)
  | Op.GreaterEqual -> eval_comparison stack ((<=) 0)
  | Op.GetAttr name ->
      let attr = match Stack.pop stack with
        Value.Array a -> get_array_attr a name
      | Value.Dict h -> get_dict_attr h name
      | _ -> failwith "Unknown object" in
      Stack.push attr stack
  | Op.Jump label -> frame.pc <- Some label
  | Op.JumpIfFalse label ->
      (match Stack.top stack with
        Value.Bool false -> ignore (Stack.pop stack); frame.pc <- Some label
      | _ -> ())
  | Op.Less -> eval_comparison stack ((>) 0)
  | Op.LessEqual -> eval_comparison stack ((>=) 0)
  | Op.MakeArray size ->
      Stack.push (Value.Array (Array.of_list (pop_args stack size))) stack
  | Op.MakeDict size ->
      let hash = Hashtbl.create 0 in
      let rec iter = function
          0 -> ()
        | n ->
            let value = Stack.pop stack in
            let key = Stack.pop stack in
            Hashtbl.replace hash key value;
            iter (n - 1) in
      iter size;
      Stack.push (Value.Dict hash) stack
  | Op.MakeUserFunction (args, ops_index) ->
      Stack.push (Value.UserFunction (args, ops_index)) stack
  | Op.MoveParam ->
      let value = Stack.pop stack in
      (match value with
        Value.String param ->
          let cmd = List.hd (Stack.top frame.pipelines) in
          DynArray.add cmd.cmd_params param
      | _ ->
          let header = "Unsupported redirect: " in
          failwith (header ^ (Builtins.string_of_value value)))
  | Op.Mul ->
      let intf n m = Value.Int (Num.mult_num n m) in
      let floatf x y = Value.Float (x *. y) in
      let rec string_intf s n accum =
        if Num.eq_num n (Num.num_of_int 0) then
          Value.String accum
        else
          string_intf s (Num.pred_num n) (accum ^ s) in
      eval_binop stack intf floatf error string_intf
  | Op.NotEqual -> eval_equality stack ((<>) 0)
  | Op.Pop -> ignore (Stack.pop stack)
  | Op.PushCommand ->
      let cmd = { cmd_params=DynArray.create (); cmd_path=None } in
      let cmds = Stack.pop frame.pipelines in
      Stack.push (cmd :: cmds) frame.pipelines
  | Op.PushConst v -> Stack.push v stack
  | Op.PushLocal name -> Stack.push (find_local env frame name) stack
  | Op.PushPipeline -> Stack.push [] frame.pipelines
  | Op.Return ->
      let value = Stack.pop stack in
      ignore (Stack.pop env.frames);
      let frame = Stack.top env.frames in
      Stack.push value frame.stack
  | Op.StoreLocal name ->
      Symboltbl.add frame.locals name (Stack.pop stack)
  | Op.StoreSubscript ->
      let index = Stack.pop stack in
      let prefix = Stack.pop stack in
      let value = Stack.top stack in
      (match prefix, index with
        Value.Array a, Value.Int (Num.Int n) -> Array.set a n value
      | Value.Dict h, _ -> Hashtbl.replace h index value
      | _ -> failwith "Invalid subscript operation")
  | Op.Sub ->
      let intf n m = Value.Int (Num.sub_num n m) in
      let floatf x y = Value.Float (x -. y) in
      eval_binop stack intf floatf error error
  | Op.Subscript ->
      let index = Stack.pop stack in
      let prefix = Stack.pop stack in
      let value = match prefix, index with
        Value.Array a, Value.Int (Num.Int n) -> Array.get a n
      | Value.Dict h, _ -> Hashtbl.find h index
      | _ -> failwith "Invalid subscript operation" in
      Stack.push value stack

  | Op.Anchor -> ()
  | Op.Label -> ()

let rec eval_env env =
  let frame = Stack.top env.frames in
  match frame.pc with
    Some op ->
      frame.pc <- Op.next_of_op op;
      eval_op env frame (Op.kind_of_op op);
      eval_env env
  | None -> ()

let eval ops =
  let frame = {
    pc=Some ops;
    locals=Symboltbl.create ();
    outer_frame=None;
    stack=Stack.create ();
    pipelines=Stack.create () } in
  let stack = Stack.create () in
  Stack.push frame stack;
  eval_env { globals=Builtins.create (); frames=stack }

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
