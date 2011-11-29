{
type lexer = {
  list_indent: int Stack.t;
  title_marks: char DynArray.t;
  mutable get_token: lexer -> Lexing.lexbuf -> Parser.token }
}

let newline = '\n'
let title_mark = ['=' '-' '`' ':' '\'' '"' '~' '^' '*' '+' '#' '<' '>']
let bullet_mark = ['-' '*' '+']
let whitespace = ' '
let contents = [^ '\n']*

rule normal lexer = shortest
    eof { Parser.EOF }
  | ("::" as s) newline {
    lexer.get_token <- preformatted;
    Parser.LINE s
  }
  | (title_mark as c) title_mark* newline {
    let stack = lexer.title_marks in
    (try
      let pos = DynArray.index_of ((=) c) stack in
      DynArray.delete_range stack 0 pos
    with
      Not_found -> DynArray.insert stack 0 c);
    Parser.UNDERLINE (DynArray.length stack)
  }
  | (whitespace* as ws) bullet_mark whitespace* (_* as text) newline {
    Parser.BULLET_ITEM ((String.length ws), text)
  }
  | ".." _* newline { lexer.get_token lexer lexbuf }
  | (_+ "::" as s) newline {
    lexer.get_token <- preformatted;
    Parser.LINE s
  }
  | (_+ as s) newline { Parser.LINE s }
  | newline+ { Parser.EMPTY }
and preformatted lexer = parse
  | (whitespace+ contents as text) newline { Parser.PREFORMATTED text }
  | newline { Parser.PREFORMATTED_EMPTY }
  | "" {
    lexer.get_token <- normal;
    Parser.EMPTY
  }

{
let make_lexer () = {
  list_indent=Stack.create ();
  title_marks=DynArray.create ();
  get_token=normal }

let token lexer lexbuf = lexer.get_token lexer lexbuf
}
(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
