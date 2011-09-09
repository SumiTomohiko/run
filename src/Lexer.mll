{
type mode = Script | Command

let mode = ref Script
}

let digit = ['0' - '9']
let alpha = ['A' - 'Z' 'a' - 'z' '_']
let alnum = alpha | digit

rule script_token = parse
    eof { Parser.EOF }
  | "as" { Parser.AS }
  | "end" { Parser.END }
  | "every" { mode := Command; Parser.EVERY }
  | "false" { Parser.FALSE }
  | "true" { Parser.TRUE }
  | ' ' { script_token lexbuf }
  | '(' { Parser.LPAR }
  | ')' { Parser.RPAR }
  | '+' { Parser.PLUS }
  | '-' { Parser.MINUS }
  | '.' { Parser.COLON }
  | '=' { Parser.EQUAL }
  | '\n' { Parser.NEWLINE }
  | alpha alnum* as s { Parser.NAME s }
  | digit+ as s { Parser.INT (Num.num_of_string s) }
and command_token = parse
    "as" { mode := Script; Parser.AS }
  | ' ' { command_token lexbuf }
  | [^' ']+ as s { Parser.PATTERN (s) }
  | '\n' { mode := Script; Parser.NEWLINE }

{
let token lexbuf =
  if !mode = Script then script_token lexbuf else command_token lexbuf
}
(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
