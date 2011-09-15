{
type mode = Script | Command
let mode = ref Script
let switch_to_script () = mode := Script
let switch_to_command () = mode := Command

let top_of_line = ref true

let rec peek_next_char s pos =
  let c = String.get s pos in
  if c = ' ' then peek_next_char s (pos + 1) else c

let heredoc_queue = Queue.create ()
}

let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = alpha | digit

rule script_token = parse
    eof { Parser.EOF }
  | "<<" (alpha alnum* as name) {
    let buf = Buffer.create 16 in
    Queue.add (name, ref buf) heredoc_queue;
    Parser.HEREDOC buf
  }
  | "!=" { Parser.NOT_EQUAL }
  | "(:" { comment 1 lexbuf }
  | "//" { Parser.DIV_DIV }
  | "<=" { Parser.LESS_EQUAL }
  | "==" { Parser.EQUAL_EQUAL }
  | ">=" { Parser.GREATER_EQUAL }
  | "as" { Parser.AS }
  | "break" { Parser.BREAK }
  | "def" { Parser.DEF }
  | "elif" { Parser.ELIF }
  | "else" { Parser.ELSE }
  | "end" { Parser.END }
  | "every" { switch_to_command (); Parser.EVERY }
  | "false" { Parser.FALSE }
  | "if" { Parser.IF }
  | "next" { Parser.NEXT }
  | "return" { Parser.RETURN }
  | "true" { Parser.TRUE }
  | "while" { Parser.WHILE }
  | ' '+ { script_token lexbuf }
  | '"' { string_token "" lexbuf }
  | '#' [^'\n']* { script_token lexbuf }
  | '(' { Parser.LPAR }
  | ')' { Parser.RPAR }
  | '*' { Parser.STAR }
  | '+' { Parser.PLUS }
  | ',' { Parser.COMMA }
  | '-' { Parser.MINUS }
  | '.' { Parser.DOT }
  | '/' { Parser.DIV }
  | ':' { Parser.COLON }
  | '<' { Parser.LESS }
  | '=' { Parser.EQUAL }
  | '>' { Parser.GREATER }
  | '[' { Parser.LBRACKET }
  | '\n' { Parser.NEWLINE }
  | ']' { Parser.RBRACKET }
  | '{' { Parser.LBRACE }
  | '}' { Parser.RBRACE }
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
and comment depth = parse
    ":)" {
      if depth = 1 then script_token lexbuf else comment (depth - 1) lexbuf
  }
  | "(:" { comment (depth + 1) lexbuf }
  | _ { comment depth lexbuf }
and heredoc name buf = parse
    ([^'\n']* as s) '\n' {
      if s = name then
        ()
      else
        (Buffer.add_string buf (s ^ "\n"); heredoc name buf lexbuf)
  }

{
let rec token lexbuf =
  if !top_of_line && not (Queue.is_empty heredoc_queue) then
    let name, buf = Queue.take heredoc_queue in
    heredoc name !buf lexbuf;
    token lexbuf
  else
    let f = match !mode with Script -> script_token | _ -> command_token in
    let tok = f lexbuf in
    top_of_line := (match tok with Parser.NEWLINE -> true | _ -> false);
    tok
}
(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
