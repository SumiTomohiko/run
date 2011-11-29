%{
let parse_inline text =
  let lexbuf = Lexing.from_string text in
  let rec loop nodes lexbuf =
    match InlineLexer.token lexbuf with
      InlineNode.Eof -> nodes
    | node -> loop (nodes @ [node]) lexbuf in
  loop [] lexbuf
%}
%token EOF EMPTY PREFORMATTED_EMPTY
%token <int> UNDERLINE
%token <string> LINE PREFORMATTED
%token <int * string> BULLET_ITEM
%type <BlockNode.t list> rst
%start rst
%right PREFORMATTED_EMPTY
%%
rst
  : blocks EOF { $1 }
  ;

blocks
  : blocks block { $1 @ $2 }
  | block { $1 }
  ;

block
  : EMPTY line UNDERLINE { [BlockNode.Title ($3, $2)] }
  | EMPTY lines { [BlockNode.Paragraph $2] }
  | PREFORMATTED_EMPTY preformatted_blocks PREFORMATTED_EMPTY {
    [BlockNode.Preformatted $2]
  }
  | EMPTY bullet_list { $2 }
  | EMPTY { [] }
  ;

bullet_list
  : bullet_list bullet_item { $1 @ [$2] }
  | bullet_item { [$1] }
  ;

bullet_item
  : BULLET_ITEM lines_opt {
    let depth, text = $1 in
    BlockNode.BulletItem (depth, (parse_inline text) @ $2)
  }
  ;

lines_opt
  : { [] }
  | lines { $1 }
  ;

lines
  : lines LINE { $1 @ (parse_inline ("\n" ^ $2)) }
  | line { $1 }
  ;

line
  : LINE { parse_inline $1 }
  ;

preformatted_blocks
  : preformatted_blocks PREFORMATTED_EMPTY preformatted_lines { $1 @ [""] @ $3 }
  | preformatted_lines { $1 }
  ;

preformatted_lines
  : preformatted_lines preformatted { $1 @ [$2] }
  | preformatted { [$1] }
  ;

preformatted
  : PREFORMATTED { $1 }
  ;

/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 */
