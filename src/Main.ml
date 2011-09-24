
let parse ch =
  let tokenizer, lexbuf = Lexer.make_tokenizer ch in
  Parser.script tokenizer lexbuf

let main () =
  let stmts = Ensure.open_in (Array.get Sys.argv 1) parse in
  Eval.eval (Compiler.compile stmts)

let _ = main ()

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
