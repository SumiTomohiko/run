
let digit = ['0' - '9']
let alpha = ['A' - 'Z' 'a' - 'z' '_']
let alnum = alpha | digit

rule token = parse
    eof { Parser.EOF }
  | "true" { Parser.TRUE }
  | "false" { Parser.FALSE }
  | digit+ as s { Parser.INT (Num.num_of_string s) }
  | '\n' { Parser.NEWLINE }
  | ' ' { token lexbuf }
  | '+' { Parser.PLUS }
  | '(' { Parser.LPAR }
  | ')' { Parser.RPAR }
  | '.' { Parser.COLON }
  | alpha alnum* as s { Parser.NAME s }
(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
