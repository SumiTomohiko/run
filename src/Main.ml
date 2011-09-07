
let parse ch = Parser.script Lexer.token (Lexing.from_channel ch)

let main () =
  let _ = Ensure.open_in (Array.get Sys.argv 1 ) parse in
  ()

let _ = main ()

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
