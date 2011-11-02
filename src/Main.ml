
let parse ch =
  let tokenizer, lexbuf = Lexer.tokenizer_of_channel ch in
  Parser.program tokenizer lexbuf

let main () =
  let src = Array.get Sys.argv 1 in
  let stmts = Ensure.open_in src parse in
  let codes = DynArray.create () in
  let index = Compiler.compile src codes stmts in
  Eval.eval (DynArray.to_array codes) index

let _ = main ()

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
