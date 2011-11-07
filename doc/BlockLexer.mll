{
type lexer = {
  list_indent: int Stack.t;
  title_marks: char DynArray.t
}
}

let newline = '\n'
let title_mark = ['=' '-' '`' ':' '\'' '"' '~' '^' '*' '+' '#' '<' '>']
let bullet_mark = ['-' '*' '+']
let whitespace = ' '

rule token lexer = shortest
    eof { Parser.EOF }
  | ("::" as s) newline { Parser.LINE s }
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
  | ".." _* newline { token lexer lexbuf }
  | (_+ as s) newline { Parser.LINE s }
  | newline+ { Parser.EMPTY }

{
let make_lexer () =
  { list_indent=Stack.create (); title_marks=DynArray.create () }
}
(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
