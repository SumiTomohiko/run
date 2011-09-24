rule token = parse
    [^'\n']* '\n' {
    let s = Lexing.lexeme lexbuf in
    match s with
      "SRC:\n" -> Parser.SRC
    | "OUT:\n" -> Parser.OUT
    | "ERR:\n" -> Parser.ERR
    | _ -> Parser.CONTENT (s)
  }
  | eof { Parser.EOF }
(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
