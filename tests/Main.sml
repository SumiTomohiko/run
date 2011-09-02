
fun main () =
let
  val instream = TextIO.openIn (List.hd (CommandLine.arguments ()))
  fun body () = TextIO.inputAll instream
  fun finally () = TextIO.closeIn instream
  val lexbuf = Lexing.createLexerString (Try.try body finally)
  val test = Parser.Test Lexer.Token lexbuf
in
  print (#src test)
end

val _ = main ();

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
