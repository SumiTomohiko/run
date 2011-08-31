
fun main () =
let
  val instream = TextIO.openIn (List.hd (CommandLine.arguments ()))
  val buf = Lexing.createLexerString (TextIO.inputAll instream)
  val _ = TextIO.closeIn instream
  val test = Parser.Test Lexer.Token buf
in
  print (#src test)
end

val _ = main ();

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
