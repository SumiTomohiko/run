
let rec pop_args stack = function
  | 0 -> []
  | n -> let last = Stack.pop stack in (pop_args stack (n - 1)) @ [last]

let call env callee args =
  match callee with
  | Core.Function f -> f args
  | Core.Method (self, f) -> f env self args
  | _ -> failwith "Object is not callable"

let find_global env = Core.Symboltbl.find env.Core.globals

let find_local env frame name =
  try
    Core.Symboltbl.find frame.Core.locals name
  with
  | Not_found -> find_global env name

let raise_unsupported_operands_error () =
  failwith "Unsupported operands"

let eval_binop stack intf floatf stringf string_intf =
  let right = Stack.pop stack in
  let left = Stack.pop stack in
  let result = match left, right with
    | Core.Int n, Core.Int m -> intf n m
    | Core.Float x, Core.Float y -> floatf x y
    | Core.String s, Core.String t -> stringf s t
    | Core.Int n, Core.String s -> string_intf s n ""
    | Core.String s, Core.Int n -> string_intf s n ""
    | _ -> raise_unsupported_operands_error () in
  Stack.push result stack

let eval_comparison stack f =
  let right = Stack.pop stack in
  let left = Stack.pop stack in
  let result = match left, right with
  | Core.Int n, Core.Int m -> compare n m
  | Core.Float f, Core.Float g -> compare f g
  | Core.String s, Core.String t -> compare s t
  | _ -> failwith "Invalid comparison" in
  Stack.push (Core.Bool (f result)) stack

let eval_equality stack f =
  let right = Stack.pop stack in
  let left = Stack.pop stack in
  let result = match left, right with
  | Core.Int n, Core.Int m -> compare n m
  | Core.Float f, Core.Float g -> compare f g
  | Core.String s, Core.String t -> compare s t
  | _ -> 1 in
  Stack.push (Core.Bool (f result)) stack

let rec make_pipes pairs prev_pair last_pair = function
  | 1 -> pairs @ [(prev_pair, last_pair)]
  | n ->
      let rfd, wfd = Unix.pipe () in
      let pair = (Some rfd, Some wfd) in
      make_pipes (pairs @ [(prev_pair, pair)]) pair last_pair (n - 1)

let dup oldfd newfd = Unix.dup2 (Option.default newfd oldfd) newfd

let close = Option.may Unix.close

let exec cmd =
  let args = cmd.Core.cmd_params in
  let prog = DynArray.get args 0 in
  Unix.execvp prog (DynArray.to_array args)

let dup_and_close oldfd newfd =
  Unix.dup2 oldfd newfd;
  Unix.close oldfd

let exec_communicate cmd pipes =
  match Unix.fork () with
  | 0 ->
      dup_and_close (fst (List.hd pipes)) Unix.stdin;
      dup_and_close (snd (List.hd (List.tl pipes))) Unix.stdout;
      let args = cmd.Core.cmd_params in
      let prog = DynArray.get args 0 in
      Unix.execv prog (DynArray.to_array args)
  | pid -> pid

let exec_command cmd (pipe1, pipe2) =
  match Unix.fork () with
  | 0 ->
      close (snd pipe1);
      close (fst pipe2);
      dup (fst pipe1) Unix.stdin;
      dup (snd pipe2) Unix.stdout;
      (match cmd.Core.cmd_stderr_redirect with
      | Some (Redirect.File (path, flags)) ->
          let fd = Unix.openfile path flags 0o644 in
          Unix.dup2 fd Unix.stderr;
          Unix.close fd
      | Some Redirect.Dup -> Unix.dup2 Unix.stdout Unix.stderr
      | None -> ());
      exec cmd
  | pid -> pid

let rec close_second_pipes = function
  | (_, pair) :: tl ->
      close (fst pair);
      close (snd pair);
      close_second_pipes tl
  | [] -> ()

let add_command frame =
  DynArray.add (Stack.top frame.Core.pipelines).Core.pl_commands

let wait_children = List.iter (fun pid -> ignore (Unix.waitpid [] pid))

let close_all_pipes = function
  | [(rfd1, wfd1); (rfd2, wfd2)] ->
      List.iter Unix.close [rfd1; wfd1; rfd2; wfd2]
  | _ -> failwith "Invalid pipes"

let exec_pipeline env pipeline last_stdout_pair read_last_output =
  let cmds = pipeline.Core.pl_commands in
  let cmd = DynArray.get cmds 0 in
  let name = DynArray.get cmd.Core.cmd_params 0 in
  try
    let f = Builtins.Command.find env.Core.builtins name in
    let ret = f env (List.tl (DynArray.to_list cmd.Core.cmd_params)) in
    close (fst last_stdout_pair);
    close (snd last_stdout_pair);
    ret
  with
  | Not_found ->
      let first_stdin_fd = match pipeline.Core.pl_first_stdin with
      | Some path -> Some (Unix.openfile path [Unix.O_RDONLY] 0)
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
  | ' '
  | '\n' -> trim (String.sub s 0 (size - 1))
  | _ -> s

let rec concat stack size s =
  if size = 0 then
    s
  else
    let v = Stack.pop stack in
    concat stack (size - 1) ((Core.string_of_value v) ^ s)

let eval_params stack n params =
  let vals = Array.of_list (pop_args stack n) in
  List.concat (List.map (Param.eval vals) params)

let jump frame dest = frame.Core.pc <- dest

let store_top_local frame = Core.Symboltbl.add frame.Core.locals

let eval_op env frame op =
  let stack = frame.Core.stack in
  let error _ = raise_unsupported_operands_error () in
  match op with
  | Op.Add ->
      let intf n m = Core.Int (Num.add_num n m) in
      let floatf x y = Core.Float (x +. y) in
      let stringf s t = Core.String (s ^ t) in
      eval_binop stack intf floatf stringf error
  | Op.Call nargs ->
      let args = pop_args frame.Core.stack nargs in
      (match Stack.pop stack with
      | Core.UserFunction (params, index) ->
          let locals = Core.Symboltbl.create () in
          let store_param param arg = Core.Symboltbl.add locals param arg in
          List.iter2 store_param params args;
          let frame = Core.make_frame index locals in
          Stack.push frame env.Core.frames
      | f -> Stack.push (call env f args) stack)
  | Op.Communicate ->
      let pipeline = Stack.pop frame.Core.pipelines in
      let pipes = [Unix.pipe (); Unix.pipe ()] in
      let cmds = DynArray.to_list pipeline.Core.pl_commands in
      let pids = List.map2 exec_communicate cmds [pipes; (List.rev pipes)] in
      close_all_pipes pipes;
      wait_children pids
  | Op.Concat size -> Stack.push (Core.String (concat stack size "")) stack
  | Op.Div ->
      let intf n m = Core.Float (Num.float_of_num (Num.div_num n m)) in
      let floatf x y = Core.Float (x /. y) in
      eval_binop stack intf floatf error error
  | Op.DivDiv ->
      let intf n m = Core.Int (Num.floor_num (Num.div_num n m)) in
      let floatf x y = Core.Float (x /. y) in
      eval_binop stack intf floatf error error
  | Op.Equal -> eval_equality stack ((=) 0)
  | Op.Exec ->
      let pipeline = Stack.pop frame.Core.pipelines in
      let last_stdout_fd = match pipeline.Core.pl_last_stdout with
      | Some (path, flags) -> Some (Unix.openfile path flags 0o644)
      | None -> None in
      let last_pair = (None, last_stdout_fd) in
      let read_last_output pid _ =
        let status = match snd (Unix.waitpid [] pid) with
        | Unix.WEXITED status -> status
        | _ -> -1 in
        status, "" in
      let status, _ = exec_pipeline env pipeline last_pair read_last_output in
      env.Core.last_status <- status
  | Op.ExecAndPush ->
      let rfd, wfd = Unix.pipe () in
      let pair = (Some rfd, Some wfd) in
      let read_last_output pid = function
      | Some fd ->
          let rec loop s pid fd =
            let i = IO.input_channel (Unix.in_channel_of_descr fd) in
            match Unix.waitpid [Unix.WNOHANG] pid with
            | 0, _ ->
                let t = try IO.nread i 1024 with IO.No_more_input -> "" in
                loop (s ^ t) pid fd
            | _, Unix.WEXITED status -> status, s
            | _, _ -> -1, s in
          loop "" pid fd
      | None -> failwith "Invalid stdout file descriptor" in
      let pipeline = Stack.pop frame.Core.pipelines in
      let status, stdout = exec_pipeline env pipeline pair read_last_output in
      env.Core.last_status <- status;
      Stack.push (Core.String (trim stdout)) stack
  | Op.Greater -> eval_comparison stack ((<) 0)
  | Op.GreaterEqual -> eval_comparison stack ((<=) 0)
  | Op.GetAttr name ->
      let v = Stack.pop stack in
      let getter = match v with
      | Core.Array _ -> Builtins.Class.Array.get_attr
      | Core.Class _ -> Builtins.Class.Class.get_attr
      | Core.Exception _ -> Builtins.Class.Exception.get_attr
      | Core.Dict _ -> Builtins.Class.Dict.get_attr
      | _ -> failwith "Unknown object" in
      Stack.push (getter env v name) stack
  | Op.Jump dest -> jump frame dest
  | Op.JumpIfException dest ->
      let expected = Stack.pop stack in
      if expected == Core.Symboltbl.find env.Core.globals "Exception" then
        jump frame dest
      else
        (match env.Core.last_exception with
        | Core.Exception (actual, _, _) when expected == actual ->
            jump frame dest
        | _ -> ())
  | Op.JumpIfFalse dest ->
      if not (Core.bool_of_value (Stack.top stack)) then begin
        ignore (Stack.pop stack);
        jump frame dest
      end
  | Op.Less -> eval_comparison stack ((>) 0)
  | Op.LessEqual -> eval_comparison stack ((>=) 0)
  | Op.MakeArray size ->
      Stack.push (Core.Array (Array.of_list (pop_args stack size))) stack
  | Op.MakeDict size ->
      let hash = Hashtbl.create 0 in
      let rec iter = function
        | 0 -> ()
        | n ->
            let value = Stack.pop stack in
            let key = Stack.pop stack in
            Hashtbl.replace hash key value;
            iter (n - 1) in
      iter size;
      Stack.push (Core.Dict hash) stack
  | Op.MakeException name ->
      Stack.push (Exception.make_exception_class name) stack
  | Op.MakeIterator ->
      let iter = match Stack.pop stack with
      | Core.Array a -> Core.ArrayIterator (ExtArray.Array.enum a)
      | Core.Dict d -> Core.DictIterator (ExtHashtbl.Hashtbl.enum d)
      | Core.Int n -> Core.IntIterator (Num.num_of_int 0, n)
      | _ -> assert false in
      Stack.push iter stack
  | Op.MakeUserFunction (args, index) ->
      Stack.push (Core.UserFunction (args, index)) stack
  | Op.MoveIterator dest ->
      (match Stack.top stack with
      | Core.ArrayIterator enum ->
          (match Enum.get enum with
          | Some v -> Stack.push v stack
          | None ->
              ignore (Stack.pop stack);
              jump frame dest)
      | Core.DictIterator enum ->
          (match Enum.get enum with
          | Some (key, value) ->
              Stack.push value stack;
              Stack.push key stack
          | None ->
              ignore (Stack.pop stack);
              jump frame dest)
      | Core.IntIterator (next, max) ->
          ignore (Stack.pop stack);
          if Num.(=/) next max then
            jump frame dest
          else
            Stack.push (Core.IntIterator (Num.succ_num next, max)) stack;
            Stack.push (Core.value_of_num next) stack
      | _ -> assert false)
  | Op.MoveParam ->
      let s = Core.string_of_value (Stack.pop stack) in
      let pipeline = Stack.top frame.Core.pipelines in
      let cmd = DynArray.last pipeline.Core.pl_commands in
      DynArray.add cmd.Core.cmd_params s
  | Op.Mul ->
      let intf n m = Core.Int (Num.mult_num n m) in
      let floatf x y = Core.Float (x *. y) in
      let rec string_intf s n accum =
        if Num.eq_num n (Num.num_of_int 0) then
          Core.String accum
        else
          string_intf s (Num.pred_num n) (accum ^ s) in
      eval_binop stack intf floatf error string_intf
  | Op.NotEqual -> eval_equality stack ((<>) 0)
  | Op.Pop -> ignore (Stack.pop stack)
  | Op.PushCommand flags ->
      let stderr_redirect = match Stack.pop stack with
      | Core.String path -> Some (Redirect.File (path, flags))
      | Core.Nil -> None
      | _ -> failwith "Invalid stderr redirect" in
      let cmd = {
        Core.cmd_params=DynArray.create ();
        Core.cmd_stderr_redirect=stderr_redirect } in
      add_command frame cmd
  | Op.PushCommandE2O ->
      let cmd = {
        Core.cmd_params=DynArray.create ();
        Core.cmd_stderr_redirect=Some Redirect.Dup } in
      add_command frame cmd
  | Op.PushCommandParams (n, params) ->
      let pipeline = Stack.top frame.Core.pipelines in
      let cmd = DynArray.last pipeline.Core.pl_commands in
      List.iter (DynArray.add cmd.Core.cmd_params) (eval_params stack n params)
  | Op.PushConst v -> Stack.push v stack
  | Op.PushLastStatus ->
      Stack.push (Core.ProcessStatus env.Core.last_status) stack
  | Op.PushLocal name -> Stack.push (find_local env frame name) stack
  | Op.PushParams (n, params) ->
      let actuals = eval_params stack n params in
      List.iter (fun s -> Stack.push (Core.String s) stack) actuals
  | Op.PushPipeline flags ->
      let stdout = match Stack.pop stack with
      | Core.String path -> Some (path, flags)
      | Core.Nil -> None
      | _ -> failwith "Invalid stdout path" in
      let stdin = match Stack.pop stack with
      | Core.String path -> Some path
      | Core.Nil -> None
      | _ -> failwith "Invalid stdin path" in
      let pipeline = {
        Core.pl_commands=DynArray.create ();
        Core.pl_first_stdin=stdin;
        Core.pl_last_stdout=stdout } in
      Stack.push pipeline frame.Core.pipelines
  | Op.Raise ->
      let e = match Stack.pop stack with
      | Core.Exception _ as e -> e
      | e ->
          let clazz = Core.Symboltbl.find env.Core.globals "Exception" in
          let create = match clazz with
          | Core.Class (_, create) -> create
          | _ -> assert false in
          create env clazz [e] in
      raise (Exception.Run_exception e)
  | Op.Reraise -> raise (Exception.Run_exception env.Core.last_exception)
  | Op.Return ->
      let value = Stack.pop stack in
      ignore (Stack.pop env.Core.frames);
      let frame = Stack.top env.Core.frames in
      Stack.push value frame.Core.stack
  | Op.StoreLastException name ->
      store_top_local frame name env.Core.last_exception
  | Op.StoreLocal name -> store_top_local frame name (Stack.pop stack)
  | Op.StoreSubscript ->
      let index = Stack.pop stack in
      let prefix = Stack.pop stack in
      let value = Stack.top stack in
      (match prefix, index with
      | Core.Array a, Core.Int (Num.Int n) -> Array.set a n value
      | Core.Dict h, _ -> Hashtbl.replace h index value
      | _ -> failwith "Invalid subscript operation")
  | Op.Sub ->
      let intf n m = Core.Int (Num.sub_num n m) in
      let floatf x y = Core.Float (x -. y) in
      eval_binop stack intf floatf error error
  | Op.Subscript ->
      let index = Stack.pop stack in
      let prefix = Stack.pop stack in
      let value = match prefix, index with
      | Core.Array a, Core.Int (Num.Int n) -> Array.get a n
      | Core.Dict h, _ -> Hashtbl.find h index
      | _ -> failwith "Invalid subscript operation" in
      Stack.push value stack

  | Op.Anchor
  | Op.Label -> assert false

let rec eval_env env =
  let frames = env.Core.frames in
  let frame = Stack.top frames in
  let pc = frame.Core.pc in
  let code = Code.code_of_index frame.Core.code in
  let ops = Code.ops_of_code code in
  if (Array.length ops) <= pc then
    ()
  else begin
    jump frame (pc + 1);
    (try
      let op = Array.get ops (frame.Core.pc - 1) in
      eval_op env frame op
    with
    | Exception.Run_exception v as e ->
        env.Core.last_exception <- v;
        let rec jump_to_except () =
          if Stack.is_empty frames then
            raise e
          else
            let frame = Stack.top frames in
            let pc = frame.Core.pc - 1 in
            let code = Code.code_of_index frame.Core.code in
            let dest = Code.dest_of_exception code pc in
            if dest < 0 then begin
              ignore (Stack.pop frames);
              jump_to_except ();
            end else
              jump frame dest in
        jump_to_except ());
    eval_env env
  end

let make_argv () =
  let argv = Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1) in
  Core.value_of_array (Array.map Core.value_of_string argv)

let make_globals () =
  let globals = Builtins.Function.create () in
  Exception.register_exceptions globals;
  Core.Symboltbl.add globals "ARGV" (make_argv ());
  globals

let eval index =
  let builtins = Builtins.Command.create () in
  eval_env (Core.create_env index (make_globals ()) builtins)

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
