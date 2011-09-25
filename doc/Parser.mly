%token EOF EMPTY UNDERLINE
%token <string> LINE
%type <BlockNode.t list> rst
%start rst
%%
rst
  : blocks EOF { $1 }
  ;

blocks
  : blocks block { $1 @ $2 }
  | block { $1 }
  ;

block
  : EMPTY line UNDERLINE { [BlockNode.Title $2] }
  | EMPTY lines { [BlockNode.Paragraph $2] }
  | EMPTY { [] }
  ;

lines
  : lines line { $1 @ $2 }
  | line { $1 }
  ;

line
  : LINE {
    let lexbuf = Lexing.from_string $1 in
    let rec loop nodes lexbuf =
      match InlineLexer.token lexbuf with
        InlineNode.Eof -> nodes
      | node -> loop (nodes @ [node]) lexbuf in
    loop [] lexbuf
  }
  ;

/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 */
