%token COMMA EOF LBRACE RBRACE SEP STAR STAR_STAR
%token <char> CHAR
%type <Node.t list> pattern
%start pattern
%%
pattern
  : path EOF { $1 }
  | SEP path EOF { [Node.Dir] @ $2 }
  | SEP EOF { [Node.Dir] }
  ;

path
  : path SEP exprs { $1 @ [Node.Dir] @ $3 }
  | exprs { $1 }
  ;

exprs
  : exprs expr { $1 @ [$2] }
  | expr { [$1] }
  ;

expr
  : CHAR { Node.Char $1 }
  | STAR { Node.Star }
  | STAR_STAR { Node.StarStar }
  | LBRACE patterns RBRACE { Node.Branch $2 }
  ;

patterns
  : patterns COMMA path { $1 @ [$3] }
  | path { [$1] }
  ;

/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 */
