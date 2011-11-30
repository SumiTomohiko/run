
type command = {
  cmd_params: string DynArray.t;
  cmd_stderr_redirect: Redirect.t option }

type pipeline = {
  pl_commands: command DynArray.t;
  pl_first_stdin: string option;
  pl_last_stdout: (string * Unix.open_flag list) option }

type frame = {
  code: Code.t;
  mutable pc: int;
  locals: Symboltbl.t;
  outer_frame: frame option;
  stack: Value.t Stack.t;
  pipelines: pipeline Stack.t }

type env = {
  codes: Code.t array;
  globals: Symboltbl.t;
  builtins: Builtins.Command.t;
  frames: frame Stack.t;
  mutable last_status: int;
  mutable last_exception: Value.t }

let make_frame code locals = {
  code=code;
  pc=0;
  locals=locals;
  outer_frame=None;
  stack=Stack.create ();
  pipelines=Stack.create () }

let create codes index =
  let frame = make_frame (Array.get codes index) (Symboltbl.create ()) in
  let frames = Stack.create () in
  Stack.push frame frames;
  {
    codes=codes;
    globals=Builtins.Function.create ();
    builtins=Builtins.Command.create ();
    frames=frames;
    last_status=0;
    last_exception=Value.Nil }

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
