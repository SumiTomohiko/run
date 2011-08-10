
fun main () =
  let
    val instream = TextIO.openIn (List.hd (CommandLine.arguments ()))
  in
    42
  end;

val _ = main ();

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
