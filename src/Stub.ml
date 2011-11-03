
let eval_string src =
  let tokenizer, lexbuf = Lexer.tokenizer_of_string src in
  let stmts = Parser.program tokenizer lexbuf in
  let codes = DynArray.create () in
  let index = Compiler.compile "<stdin>" "<module>" codes stmts in
  Eval.eval (DynArray.to_array codes) index

let _ = Callback.register "eval_string" eval_string

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
