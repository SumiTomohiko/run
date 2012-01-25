
let parse path ch =
  let tokenizer, lexbuf = Lexer.tokenizer_of_channel ch in
  lexbuf.Lexing.lex_curr_p <- {
    Lexing.pos_fname=path;
    Lexing.pos_lnum=1;
    Lexing.pos_bol=0;
    Lexing.pos_cnum=0 };
  Parser.program tokenizer lexbuf

let rec print_traceback = function
  | (path, name, lineno) :: tl ->
      Printf.eprintf "  File \"%s\", line %d, in %s\n" path lineno name;
      print_traceback tl
  | [] -> ()

let read_exception = function
  | Core.Exception (Core.Class (name, _), tb, msg) -> name, tb, msg
  | _ -> assert false

let main () =
  let path = Array.get Sys.argv 1 in
  let stmts = Ensure.open_in path (parse path) in
  let index = Compiler.compile path "<module>" stmts in
  try
    Eval.eval index
  with
  | Exception.Run_exception e ->
      let name, tb, msg = read_exception e in
      Printf.eprintf "Traceback (most recent call last):\n";
      print_traceback tb;
      Printf.eprintf "%s: %s\n" name (Core.string_of_value msg)

let _ = main ()

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
