
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

let main () =
  let path = Array.get Sys.argv 1 in
  let stmts = Ensure.open_in path (parse path) in
  let codes = DynArray.create () in
  let index = Compiler.compile path "<module>" codes stmts in
  try
    Eval.eval (DynArray.to_array codes) index
  with
  | Exception.Run_exception (tb, e) ->
      Printf.eprintf "Traceback (most recent call last):\n";
      print_traceback tb;
      Printf.eprintf "Exception: %s\n" (Value.string_of_value e)

let _ = main ()

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
