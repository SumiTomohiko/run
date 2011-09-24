
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = alpha | digit

rule script_token heredoc_queue = parse
    eof { Parser.EOF }
  | "<<" (alpha alnum* as name) {
    let buf = Buffer.create 16 in
      (match heredoc_queue with
        Some queue -> Queue.add (name, buf) queue
      | None -> ());
      Parser.HEREDOC buf
  }
  | "!=" { Parser.NOT_EQUAL }
  | "(:" { comment heredoc_queue 1 lexbuf }
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
  | "every" { Parser.EVERY }
  | "false" { Parser.FALSE }
  | "if" { Parser.IF }
  | "next" { Parser.NEXT }
  | "return" { Parser.RETURN }
  | "true" { Parser.TRUE }
  | "while" { Parser.WHILE }
  | ' '+ { script_token heredoc_queue lexbuf }
  | '"' { Parser.STRING (string_token "" lexbuf) }
  | '#' [^'\n']* { script_token heredoc_queue lexbuf }
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
  | alpha alnum* as s { Parser.NAME s }
  | digit+ '.' digit+ as s { Parser.FLOAT (float_of_string s) }
  | digit+ as s { Parser.INT (Num.num_of_string s) }
and command_token heredoc_queue = parse
    "->" { Parser.RIGHT_ARROW }
  | "->>" { Parser.RIGHT_RIGHT_ARROW }
  | "<-" { Parser.LEFT_ARROW }
  | "<->" { Parser.LEFT_RIGHT_ARROW }
  | "=>" { Parser.RIGHT_ARROW2 }
  | "=>>" { Parser.RIGHT_RIGHT_ARROW2 }
  | "as" { Parser.AS }
  | "err->" { Parser.ERR_RIGHT_ARROW }
  | "err->>" { Parser.ERR_RIGHT_RIGHT_ARROW }
  | "err->out" { Parser.ERR_RIGHT_ARROW_OUT }
  | "out->err" { Parser.OUT_RIGHT_ARROW_ERR }
  | '"' { Parser.PATTERN (string_token "" lexbuf) }
  | ' '+ { command_token heredoc_queue lexbuf }
  | '@' { Parser.AT }
  | '\n' { Parser.NEWLINE }
  | [^'"' '@' ' ' '\n']+ as s { Parser.PATTERN s }
and string_token s = parse
    '"' { s }
  | [^'"']* as t { string_token (s ^ t) lexbuf }
and comment heredoc_queue depth = parse
    ":)" {
      match depth with
        1 -> script_token heredoc_queue lexbuf
      | _ -> comment heredoc_queue (depth - 1) lexbuf
  }
  | "(:" { comment heredoc_queue (depth + 1) lexbuf }
  | _ { comment heredoc_queue depth lexbuf }
and heredoc name buf = parse
    ([^'\n']* as s) '\n' {
      if s = name then
        ()
      else begin
        Buffer.add_string buf (s ^ "\n");
        heredoc name buf lexbuf
      end
  }

{
let read_first_string s =
  let pos = try String.index s ' ' with Not_found _ -> 0 in
  String.sub s 0 pos

let rec peek_next_char s pos =
  let c = String.get s pos in
  if c = ' ' then peek_next_char s (pos + 1) else c

let read_heredoc heredoc_queue lexbuf =
  let name, buf = Queue.take heredoc_queue in
  heredoc name buf lexbuf

let try_expr line =
  try
    ignore (Parser.script (script_token None) (Lexing.from_string line));
    true
  with
    Failure _
  | Parser.Error -> false

let try_comment line =
  try
    ignore (script_token None (Lexing.from_string line));
    false
  with
    Failure _ -> true
  | Parser.Error -> false

let try_keyword line =
  try
    match script_token None (Lexing.from_string line) with
      Parser.BREAK
    | Parser.DEF
    | Parser.ELIF
    | Parser.ELSE
    | Parser.END
    | Parser.EVERY
    | Parser.IF
    | Parser.NEXT
    | Parser.RETURN
    | Parser.WHILE -> true
    | _ -> false
  with
    Failure _
  | Parser.Error -> false

type mode = Script | Command | Comment

let determine_mode line =
  if (try_expr line) || (try_keyword line) then
    Script
  else if try_comment line then
    Comment
  else
    Command

type lexer = {
  mutable buffer: string;
  (**
   * If top_of_line is true, it indicates both of:
   * 1. Current position is top of a line.
   * 2. We don't still know that this line is Script or Command.
   *)
  mutable top_of_line: bool;
  mutable mode: mode;
  heredoc_queue: (string * Buffer.t) Queue.t
}

let make_tokenizer ch =
  let lexer = {
    buffer="";
    top_of_line=true;
    mode=Script;
    heredoc_queue=Queue.create () } in
  let fill s size =
    if lexer.buffer = "" then
      try
        lexer.buffer <- (input_line ch) ^ "\n"
      with
        End_of_file -> ()
    else
      ();

    let rec loop s size pos =
      if (size = pos) || (lexer.buffer = "") then
        pos
      else
        let buf = lexer.buffer in
        String.set s pos (String.get buf 0);
        lexer.buffer <- String.sub buf 1 ((String.length buf) - 1);
        loop s size (pos + 1) in
    loop s size 0 in

  let rec token lexbuf =
    if lexer.top_of_line && not (Queue.is_empty lexer.heredoc_queue) then begin
      read_heredoc lexer.heredoc_queue lexbuf;
      token lexbuf
    end else if not lexer.top_of_line then begin
      let tok = match lexer.mode with
        Script -> script_token (Some lexer.heredoc_queue) lexbuf
      | Command -> command_token (Some lexer.heredoc_queue) lexbuf
      | _ -> comment (Some lexer.heredoc_queue) 0 lexbuf in
      lexer.top_of_line <- (match tok with Parser.NEWLINE -> true | _ -> false);
      (match tok with
        Parser.AS
      | Parser.NEWLINE -> lexer.mode <- Script
      | Parser.EVERY -> lexer.mode <- Command
      | _ -> ());
      tok
    end else begin
      lexer.buffer <- (try
        (input_line ch) ^ "\n"
      with
        End_of_file -> "");
      lexer.mode <- determine_mode lexer.buffer;
      lexer.top_of_line <- false;
      token lexbuf
    end in
  token, Lexing.from_function fill
}
(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
