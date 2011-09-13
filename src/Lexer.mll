{
type mode = Script | Command

let mode = ref Script
let switch_to_script () = mode := Script
let switch_to_command () = mode := Command
let top_of_line = ref true
let rec peek_next_char s pos =
  let c = String.get s pos in
  if c = ' ' then peek_next_char s (pos + 1) else c
}

let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = alpha | digit

rule script_token = parse
    eof { Parser.EOF }
  | "//" { Parser.DIV_DIV }
  | "as" { Parser.AS }
  | "end" { Parser.END }
  | "every" { switch_to_command (); Parser.EVERY }
  | "false" { Parser.FALSE }
  | "true" { Parser.TRUE }
  | ' '+ { script_token lexbuf }
  | '"' { string_token "" lexbuf }
  | '(' { Parser.LPAR }
  | ')' { Parser.RPAR }
  | '*' { Parser.STAR }
  | '+' { Parser.PLUS }
  | ',' { Parser.COMMA }
  | '-' { Parser.MINUS }
  | '.' { Parser.COLON }
  | '/' { Parser.DIV }
  | '=' { Parser.EQUAL }
  | '[' { Parser.LBRACKET }
  | '\n' { Parser.NEWLINE }
  | ']' { Parser.RBRACKET }
  | alpha alnum* as s {
    if not !top_of_line then
      Parser.NAME s
    else
      let buf = lexbuf.Lexing.lex_buffer in
      let pos = lexbuf.Lexing.lex_curr_pos in
      match peek_next_char buf pos with
        '('
      | '['
      | '=' -> Parser.NAME s
      | _ -> (switch_to_command (); Parser.PATTERN s)
  }
  | digit+ '.' digit+ as s { Parser.FLOAT (float_of_string s) }
  | digit+ alpha+ alnum* as s { switch_to_command (); Parser.PATTERN s }
  | digit+ as s { Parser.INT (Num.num_of_string s) }
and command_token = parse
    "as" { switch_to_script (); Parser.AS }
  | ' ' { command_token lexbuf }
  | '\n' { switch_to_script (); Parser.NEWLINE }
  | [^' ' '\n']+ as s { Parser.PATTERN s }
and string_token s = parse
    '"' { Parser.STRING s }
  | [^'"']* as t { string_token (s ^ t) lexbuf }

{
let token lexbuf =
  let f = match !mode with Script -> script_token | _ -> command_token in
  let tok = f lexbuf in
  top_of_line := (match tok with Parser.NEWLINE -> true | _ -> false);
  tok
}
(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
