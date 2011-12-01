
let eval_string src =
  let tokenizer, lexbuf = Lexer.tokenizer_of_string src in
  let stmts = Parser.program tokenizer lexbuf in
  Eval.eval (Compiler.compile "<stdin>" "<module>" stmts)

let _ = Callback.register "eval_string" eval_string

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
