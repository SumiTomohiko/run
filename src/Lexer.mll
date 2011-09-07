rule token = parse
    eof { Parser.EOF }
  | "true" { Parser.TRUE }
  | "false" { Parser.FALSE }
  | ['0' - '9'] { Parser.INT (Num.num_of_string (Lexing.lexeme lexbuf)) }
(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
