%token COMMA EOF LBRACE RBRACE STAR STAR_STAR
%token <char> CHAR
%type <Node.t list> pattern
%start pattern
%%
pattern
  : body EOF { $1 }
  ;

body
  : exprs { $1 }
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
  : patterns COMMA body { $1 @ [$3] }
  | body { [$1] }
  ;

/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 */
