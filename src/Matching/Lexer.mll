
rule token = parse
    '*' { Parser.STAR }
  | "**" { Parser.STAR_STAR }
  | '{' { Parser.LBRACE }
  | '}' { Parser.RBRACE }
  | ',' { Parser.COMMA }
  | '/' { Parser.SEP }
  | [^'\n' ' '] as c { Parser.CHAR c }
  | eof | ' ' | "" { Parser.EOF }

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
