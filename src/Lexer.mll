{
type mode = Pipeline | Comment | Script | String
type lexer = {
  mutable buffer: string;
  mode_stack: mode Stack.t;
  heredoc_queue: (string * Buffer.t) Queue.t;
  ahead_tokens: Parser.token Queue.t
}
}

let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = alpha | digit
let name = alpha alnum*

rule script_token lexer = parse
    eof { Parser.EOF }
  | "<<" (name as name) {
    let buf = Buffer.create 16 in
    Queue.add (name, buf) lexer.heredoc_queue;
    Parser.HEREDOC buf
  }
  | "!=" { Parser.NOT_EQUAL }
  | "$(" { Parser.DOLLER_LPAR }
  | "$?" { Parser.DOLLER_QUESTION }
  | "(:" { comment lexer 1 lexbuf }
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
  | ' '+ { script_token lexer lexbuf }
  | '"' { Parser.DOUBLE_QUOTE }
  | '#' [^'\n']* { script_token lexer lexbuf }
  | '$' (digit+ as s) { Parser.DOLLER_NUMBER (int_of_string s) }
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
  | name as s { Parser.NAME s }
  | digit+ '.' digit+ as s { Parser.FLOAT (float_of_string s) }
  | digit+ as s { Parser.INT (Num.num_of_string s) }
and pipeline_token lexer = parse
  | eof { Parser.EOF }
  | "${" { Parser.DOLLER_LBRACE }
  | "<->" { Parser.LEFT_RIGHT_ARROW }
  | "=>" { Parser.RIGHT_ARROW2 }
  | "=>>" { Parser.RIGHT_RIGHT_ARROW2 }
  | ">>" { Parser.GREATER2 }
  | "as" { Parser.AS }
  | "err->" { Parser.ERR_RIGHT_ARROW }
  | "err->>" { Parser.ERR_RIGHT_RIGHT_ARROW }
  | "err->out" { Parser.ERR_RIGHT_ARROW_OUT }
  | "out->err" { Parser.OUT_RIGHT_ARROW_ERR }
  | ' '+ { pipeline_token lexer lexbuf }
  | '"' {
    Parser.MATCHING_PATTERN [Matching.Main.Static (string_param "" lexbuf)]
  }
  | ')' { Parser.RPAR }
  | '<' { Parser.LESS }
  | '>' { Parser.GREATER }
  | '\n' { Parser.NEWLINE }
  | '|' { Parser.BAR }
  | '$' (digit+ as s) { Parser.DOLLER_NUMBER (int_of_string s) }
  | "" { Parser.MATCHING_PATTERN (Matching.Main.compile lexbuf) }
and string_param s = parse
  | '"' { s }
  | _ as c { string_param (s ^ (String.make 1 c)) lexbuf }
and string_token lexer s = parse
    '"' {
    Queue.add Parser.DOUBLE_QUOTE lexer.ahead_tokens;
    Parser.STRING s }
  | "${" {
    Queue.add Parser.DOLLER_LBRACE lexer.ahead_tokens;
    Parser.STRING s }
  | _ as c { string_token lexer (s ^ (String.make 1 c)) lexbuf }
and comment lexer depth = parse
    ":)" {
      match depth with
        1 -> script_token lexer lexbuf
      | _ -> comment lexer (depth - 1) lexbuf
  }
  | "(:" { comment lexer (depth + 1) lexbuf }
  | _ { comment lexer depth lexbuf }
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

let make_lexer () =
  let lexer = {
    buffer="";
    mode_stack=Stack.create ();
    heredoc_queue=Queue.create ();
    ahead_tokens=Queue.create () } in
  (* Script at bottom of a stack is a sentinel to detect eof *)
  Stack.push Script lexer.mode_stack;
  lexer

let drop_top stack = ignore (Stack.pop stack)

let next_token_of_lexbuf lexer lexbuf =
  let mode_stack = lexer.mode_stack in
  let tok = if not (Queue.is_empty lexer.ahead_tokens) then
    Queue.take lexer.ahead_tokens
  else
    match Stack.top mode_stack with
    | Script -> script_token lexer lexbuf
    | Pipeline -> pipeline_token lexer lexbuf
    | String -> string_token lexer "" lexbuf
    | Comment -> comment lexer 0 lexbuf in
  (match tok with
  | Parser.DOLLER_LBRACE
  | Parser.LBRACE
  | Parser.LPAR -> Stack.push Script mode_stack
  | Parser.DOLLER_LPAR
  | Parser.EVERY -> Stack.push Pipeline mode_stack
  | Parser.DOUBLE_QUOTE ->
      (match Stack.top mode_stack with
      | String -> drop_top mode_stack
      | _ -> Stack.push String mode_stack)
  | Parser.AS
  | Parser.NEWLINE
  | Parser.RBRACE
  | Parser.RPAR -> drop_top mode_stack
  | _ -> ());
  tok

let try_expr line =
  let lexer = make_lexer () in
  Stack.push Script lexer.mode_stack;
  let f = next_token_of_lexbuf lexer in
  try
    ignore (Parser.program f (Lexing.from_string line));
    true
  with
    Failure _
  | Parser.Error -> false

let try_comment line =
  let lexer = make_lexer () in
  try
    ignore (script_token lexer (Lexing.from_string line));
    false
  with
    Failure _ -> true
  | Parser.Error -> false

let try_keyword line =
  let lexer = make_lexer () in
  try
    match script_token lexer (Lexing.from_string line) with
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

let determine_mode line =
  if (try_expr line) || (try_keyword line) then
    Script
  else if try_comment line then
    Comment
  else
    Pipeline

let tokenizer_of_string src =
  let f = match determine_mode src with
    Script -> script_token
  | Pipeline -> pipeline_token
  | _ -> failwith "Comment is unsupported." in
  f (make_lexer ()), (Lexing.from_string src)

let tokenizer_of_channel ch =
  let lexer = make_lexer () in
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
    let mode_stack = lexer.mode_stack in
    let top_of_line = (Stack.length mode_stack) = 1 in
    if top_of_line && not (Queue.is_empty lexer.heredoc_queue) then begin
      read_heredoc lexer.heredoc_queue lexbuf;
      token lexbuf
    end else if not top_of_line then
      next_token_of_lexbuf lexer lexbuf
    else begin
      lexer.buffer <- (try
        (input_line ch) ^ "\n"
      with
        End_of_file -> "");
      Stack.push (determine_mode lexer.buffer) mode_stack;
      token lexbuf
    end in
  token, Lexing.from_function fill
}
(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
