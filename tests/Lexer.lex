rule Token = parse
    [^`\n`]* `\n` {
    let
      val s = Lexing.getLexeme lexbuf
    in
      case s of
        "SRC:\n" => Parser.SRC
      | "OUT:\n" => Parser.OUT
      | _ => Parser.CONTENT (s)
    end
  }
  | eof { Parser.EOF }
  ;
(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
