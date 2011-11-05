
type command = {
  cmd_params: string DynArray.t;
  cmd_stderr_redirect: Redirect.t option
}

type pipeline = {
  pl_commands: command DynArray.t;
  pl_first_stdin: string option;
  pl_last_stdout: (string * Unix.open_flag list) option
}

type frame = {
  code: Code.t;
  mutable pc: int;
  locals: Symboltbl.t;
  outer_frame: frame option;
  stack: Value.t Stack.t;
  pipelines: pipeline Stack.t
}

type env = {
  codes: Code.t array;
  globals: Symboltbl.t;
  builtins: Builtins.Command.t;
  frames: frame Stack.t;
  mutable last_status: int;
  mutable last_exception: Value.t }

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

let array_expand _ self _ =
  match self with
    Value.Array a ->
      let strings = Array.to_list (Array.map Value.string_of_value a) in
      Value.String (String.concat " " strings)
  | _ -> failwith "self must be Array"

let dict_expand _ self _ =
  match self with
    Value.Dict h ->
      let f key value init =
        let s = Value.string_of_value key in
        let t = Value.string_of_value value in
        (s ^ " " ^ t) :: init in
      Value.String (String.concat " " (Hashtbl.fold f h []))
  | _ -> failwith "self must be Dict"

let get_dict_attr env v = function
  | "expand" -> Value.Method (v, dict_expand env)
  | name -> failwith ("AttributeError: " ^ name)

let get_exception_attr _ v = function
  | "message" ->
      (match v with
      | Value.Exception (_, _, msg) -> msg
      | _ -> assert false)
  | name -> failwith (Printf.sprintf "TODO: Raise AttributeError of %s" name)

let get_class_attr env v = function
  | "new" ->
      (match v with
      | Value.Class (_, f) -> Value.Method (v, f)
      | _ -> assert false)
  | _ -> failwith "TODO: Raise AttributeError"

let get_array_attr env v = function
  | "size" ->
      (match v with
      | Value.Array a -> Value.Int (Num.num_of_int (Array.length a))
      | _ -> assert false)
  | "expand" -> Value.Method (v, array_expand env)
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
    1 -> pairs @ [(prev_pair, last_pair)]
  | n ->
      let rfd, wfd = Unix.pipe () in
      let pair = (Some rfd, Some wfd) in
      make_pipes (pairs @ [(prev_pair, pair)]) pair last_pair (n - 1)

let dup oldfd newfd = Unix.dup2 (Option.default newfd oldfd) newfd

let close = Option.may Unix.close

let exec cmd =
  let args = cmd.cmd_params in
  let prog = DynArray.get args 0 in
  Unix.execvp prog (DynArray.to_array args)

let dup_and_close oldfd newfd =
  Unix.dup2 oldfd newfd;
  Unix.close oldfd

let exec_communicate cmd pipes =
  match Unix.fork () with
    0 ->
      dup_and_close (fst (List.hd pipes)) Unix.stdin;
      dup_and_close (snd (List.hd (List.tl pipes))) Unix.stdout;
      let args = cmd.cmd_params in
      let prog = DynArray.get args 0 in
      Unix.execv prog (DynArray.to_array args)
  | pid -> pid

let exec_command cmd (pipe1, pipe2) =
  match Unix.fork () with
    0 ->
      close (snd pipe1);
      close (fst pipe2);
      dup (fst pipe1) Unix.stdin;
      dup (snd pipe2) Unix.stdout;
      (match cmd.cmd_stderr_redirect with
        Some (Redirect.File (path, flags)) ->
          let fd = Unix.openfile path flags 0o644 in
          Unix.dup2 fd Unix.stderr;
          Unix.close fd
      | Some Redirect.Dup -> Unix.dup2 Unix.stdout Unix.stderr
      | None -> ());
      exec cmd
  | pid -> pid

let rec close_second_pipes = function
    (_, pair) :: tl ->
      close (fst pair);
      close (snd pair);
      close_second_pipes tl
  | [] -> ()

let add_command frame = DynArray.add (Stack.top frame.pipelines).pl_commands

let wait_children = List.iter (fun pid -> ignore (Unix.waitpid [] pid))

let close_all_pipes = function
    [(rfd1, wfd1); (rfd2, wfd2)] ->
      List.iter Unix.close [rfd1; wfd1; rfd2; wfd2]
  | _ -> failwith "Invalid pipes"

let exec_pipeline env pipeline last_stdout_pair read_last_output =
  let cmds = pipeline.pl_commands in
  let cmd = DynArray.get cmds 0 in
  let name = DynArray.get cmd.cmd_params 0 in
  try
    let f = Builtins.Command.find env.builtins name in
    let ret = f (List.tl (DynArray.to_list cmd.cmd_params)) in
    close (fst last_stdout_pair);
    close (snd last_stdout_pair);
    ret
  with
    Not_found ->
      let first_stdin_fd = match pipeline.pl_first_stdin with
        Some path -> Some (Unix.openfile path [Unix.O_RDONLY] 0)
      | None -> None in
      let first_pair = (first_stdin_fd, None) in
      let num_cmds = DynArray.length cmds in
      let pipes = make_pipes [] first_pair last_stdout_pair num_cmds in
      let pids = List.map2 exec_command (DynArray.to_list cmds) pipes in
      close_second_pipes (List.tl (List.rev pipes));
      close (snd last_stdout_pair);
      let fd = fst last_stdout_pair in
      let output = read_last_output (List.hd (List.rev pids)) fd in
      close fd;
      close first_stdin_fd;
      wait_children (List.tl (List.rev pids));
      output

let rec trim s =
  let size = String.length s in
  match String.get s (size - 1) with
    ' '
  | '\n' -> trim (String.sub s 0 (size - 1))
  | _ -> s

let rec concat stack size s =
  if size = 0 then
    s
  else
    let v = Stack.pop stack in
    concat stack (size - 1) ((Value.string_of_value v) ^ s)

let eval_params stack n params =
  let vals = Array.of_list (pop_args stack n) in
  List.concat (List.map (Param.eval vals) params)

let make_frame code locals = {
  code=code;
  pc=0;
  locals=locals;
  outer_frame=None;
  stack=Stack.create ();
  pipelines=Stack.create () }

let rec make_traceback accum frames =
  if Stack.is_empty frames then
    accum
  else
    let frame = Stack.pop frames in
    let code = frame.code in
    let path = Code.path_of_code code in
    let name = Code.name_of_code code in
    let lineno = Code.lineno_of_pc code (frame.pc - 1) in
    make_traceback ((path, name, lineno) :: accum) frames

let jump frame dest = frame.pc <- dest

let store_top_local frame = Symboltbl.add frame.locals

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
      (match Stack.pop stack with
      | Value.UserFunction (params, index) ->
          let locals = Symboltbl.create () in
          let store_param param arg = Symboltbl.add locals param arg in
          List.iter2 store_param params args;
          let frame = make_frame (Array.get env.codes index) locals in
          Stack.push frame env.frames
      | f -> Stack.push (call env f args) stack)
  | Op.Communicate ->
      let pipeline = Stack.pop frame.pipelines in
      let pipes = [Unix.pipe (); Unix.pipe ()] in
      let cmds = DynArray.to_list pipeline.pl_commands in
      let pids = List.map2 exec_communicate cmds [pipes; (List.rev pipes)] in
      close_all_pipes pipes;
      wait_children pids
  | Op.Concat size -> Stack.push (Value.String (concat stack size "")) stack
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
      let last_stdout_fd = match pipeline.pl_last_stdout with
        Some (path, flags) -> Some (Unix.openfile path flags 0o644)
      | None -> None in
      let last_pair = (None, last_stdout_fd) in
      let read_last_output pid _ =
        let status = match snd (Unix.waitpid [] pid) with
          Unix.WEXITED status -> status
        | _ -> -1 in
        status, "" in
      let status, _ = exec_pipeline env pipeline last_pair read_last_output in
      env.last_status <- status
  | Op.ExecAndPush ->
      let rfd, wfd = Unix.pipe () in
      let pair = (Some rfd, Some wfd) in
      let read_last_output pid = function
        Some fd ->
          let rec loop s pid fd =
            let i = IO.input_channel (Unix.in_channel_of_descr fd) in
            match Unix.waitpid [Unix.WNOHANG] pid with
              0, _ ->
                let t = try IO.nread i 1024 with IO.No_more_input -> "" in
                loop (s ^ t) pid fd
            | _, Unix.WEXITED status -> status, s
            | _, _ -> -1, s in
          loop "" pid fd
      | None -> failwith "Invalid stdout file descriptor" in
      let pipeline = Stack.pop frame.pipelines in
      let status, stdout = exec_pipeline env pipeline pair read_last_output in
      env.last_status <- status;
      Stack.push (Value.String (trim stdout)) stack
  | Op.Greater -> eval_comparison stack ((<) 0)
  | Op.GreaterEqual -> eval_comparison stack ((<=) 0)
  | Op.GetAttr name ->
      let v = Stack.pop stack in
      let getter = match v with
      | Value.Array _ -> get_array_attr
      | Value.Class _ -> get_class_attr
      | Value.Exception _ -> get_exception_attr
      | Value.Dict _ -> get_dict_attr
      | _ -> failwith "Unknown object" in
      Stack.push (getter env v name) stack
  | Op.Jump dest -> jump frame dest
  | Op.JumpIfException dest ->
      let expected = Stack.pop stack in
      if expected == Symboltbl.find env.globals "Exception" then
        jump frame dest
      else
        (match env.last_exception with
        | Value.Exception (actual, _, _) when expected == actual ->
            jump frame dest
        | _ -> ())
  | Op.JumpIfFalse dest ->
      if not (Value.bool_of_value (Stack.top stack)) then begin
        ignore (Stack.pop stack);
        jump frame dest
      end
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
  | Op.MakeUserFunction (args, index) ->
      Stack.push (Value.UserFunction (args, index)) stack
  | Op.MoveParam ->
      let s = Value.string_of_value (Stack.pop stack) in
      let cmd = DynArray.last (Stack.top frame.pipelines).pl_commands in
      DynArray.add cmd.cmd_params s
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
  | Op.PushCommand flags ->
      let stderr_redirect = match Stack.pop stack with
        Value.String path -> Some (Redirect.File (path, flags))
      | Value.Nil -> None
      | _ -> failwith "Invalid stderr redirect" in
      let cmd = {
        cmd_params=DynArray.create (); cmd_stderr_redirect=stderr_redirect } in
      add_command frame cmd
  | Op.PushCommandE2O ->
      let cmd = {
        cmd_params=DynArray.create ();
        cmd_stderr_redirect=Some Redirect.Dup } in
      add_command frame cmd
  | Op.PushCommandParams (n, params) ->
      let cmd = DynArray.last (Stack.top frame.pipelines).pl_commands in
      List.iter (DynArray.add cmd.cmd_params) (eval_params stack n params)
  | Op.PushConst v -> Stack.push v stack
  | Op.PushLastStatus ->
      Stack.push (Value.ProcessStatus env.last_status) stack
  | Op.PushLocal name -> Stack.push (find_local env frame name) stack
  | Op.PushParams (n, params) ->
      let actuals = eval_params stack n params in
      List.iter (fun s -> Stack.push (Value.String s) stack) actuals
  | Op.PushPipeline flags ->
      let stdout = match Stack.pop stack with
        Value.String path -> Some (path, flags)
      | Value.Nil -> None
      | _ -> failwith "Invalid stdout path" in
      let stdin = match Stack.pop stack with
        Value.String path -> Some path
      | Value.Nil -> None
      | _ -> failwith "Invalid stdin path" in
      let pipeline = {
        pl_commands=DynArray.create ();
        pl_first_stdin=stdin;
        pl_last_stdout=stdout } in
      Stack.push pipeline frame.pipelines
  | Op.Raise ->
      let e = match Stack.pop stack with
      | Value.Exception _ as e -> e
      | e ->
          let clazz = Symboltbl.find env.globals "Exception" in
          let create = match clazz with
          | Value.Class (_, create) -> create
          | _ -> assert false in
          create clazz [e] in
      raise (Exception.Run_exception e)
  | Op.Reraise -> raise (Exception.Run_exception env.last_exception)
  | Op.Return ->
      let value = Stack.pop stack in
      ignore (Stack.pop env.frames);
      let frame = Stack.top env.frames in
      Stack.push value frame.stack
  | Op.StoreLastException name ->
      store_top_local frame name env.last_exception
  | Op.StoreLocal name -> store_top_local frame name (Stack.pop stack)
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

  | Op.Anchor
  | Op.Label -> assert false

let rec eval_env env =
  let frames = env.frames in
  let frame = Stack.top frames in
  let pc = frame.pc in
  let ops = Code.ops_of_code frame.code in
  if (Array.length ops) <= pc then
    ()
  else begin
    jump frame (pc + 1);
    (try
      let op = Array.get ops (frame.pc - 1) in
      eval_op env frame op
    with
    | Exception.Run_exception v as e ->
        env.last_exception <- v;
        let rec jump_to_except () =
          if Stack.is_empty frames then
            raise e
          else
            let frame = Stack.top frames in
            let dest = Code.dest_of_exception frame.code (frame.pc - 1) in
            if dest < 0 then begin
              ignore (Stack.pop frames);
              jump_to_except ();
            end else
              jump frame dest in
        jump_to_except ());
    eval_env env
  end

let make_exception env self args =
  let tb = make_traceback [] (Stack.copy env.frames) in
  match args with
  | [] -> Value.Exception (self, tb, Value.Nil)
  | [msg] -> Value.Exception (self, tb, msg)
  | _ -> failwith "TODO: Raise ArgumentError"

let add_exception env tbl name =
  Symboltbl.add tbl name (Value.Class (name, make_exception env))

let make_globals env =
  let tbl = env.globals in
  let exceptions = ["Exception"; "ArgumentError"; "IndexError"] in
  List.iter (add_exception env tbl) exceptions

let eval codes index =
  let frame = make_frame (Array.get codes index) (Symboltbl.create ()) in
  let frames = Stack.create () in
  Stack.push frame frames;
  let env = {
    codes=codes;
    globals=Builtins.Function.create ();
    builtins=Builtins.Command.create ();
    frames=frames;
    last_status=0;
    last_exception=Value.Nil } in
  make_globals env;
  eval_env env

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
