%{
let rec last = function
    [] -> None
  | [hd] -> Some hd
  | hd :: tl -> last tl
let add_return_nil stmts =
  stmts @ match last stmts with
    Some (Node.Return _) -> []
  | None
  | _ -> [Node.Return (Node.Const Value.Nil)]
%}
%token AS AT BREAK COLON COMMA DEF DIV DIV_DIV DOT ELIF ELSE END EOF EQUAL
%token EQUAL_EQUAL EVERY FALSE GREATER GREATER_EQUAL IF LBRACE LBRACKET LESS
%token LESS_EQUAL LPAR MINUS NEWLINE NEXT NOT_EQUAL PLUS RBRACE RBRACKET RETURN
%token RIGHT_ARROW RPAR STAR TRUE WHILE
%token <Num.num> INT
%token <float> FLOAT
%token <string> NAME PATTERN STRING
%token <Buffer.t> HEREDOC
%start script
%type <Node.stmt list> script
%%
script  : stmts EOF { $1 }
;

stmts : stmts NEWLINE stmt { $1 @ [$3] }
      | stmt { [$1] }
;

stmt  : expr { Node.Expr $1 }
      | DEF NAME LPAR names RPAR stmts END {
  let stmts = add_return_nil $6 in
  Node.UserFunction { Node.uf_name=$2; Node.uf_args=$4; Node.uf_stmts=stmts }
}
      | DEF NAME LPAR RPAR stmts END {
  let stmts = add_return_nil $5 in
  Node.UserFunction { Node.uf_name=$2; Node.uf_args=[]; Node.uf_stmts=stmts }
}
      | EVERY patterns AS names NEWLINE stmts END {
  Node.Every { Node.patterns=$2; Node.names=$4; Node.stmts=$6 }
}
      | IF expr NEWLINE stmts END { Node.If ($2, $4, []) }
      | IF expr NEWLINE stmts ELSE stmts END { Node.If ($2, $4, $6) }
      | IF expr NEWLINE stmts elif END { Node.If ($2, $4, [$5]) }
      | WHILE expr NEWLINE stmts END { Node.While ($2, $4) }
      | NEXT { Node.Next }
      | BREAK { Node.Break }
      | RETURN expr { Node.Return $2 }
      | pipeline { Node.Pipeline $1 }
      | { Node.Empty }
;

pipeline  : patterns RIGHT_ARROW pipeline {
  ($1, Some (Node.Write None)) :: $3
}
          | patterns RIGHT_ARROW AT PATTERN {
  [($1, Some (Node.Write (Some $4)))]
}
          | patterns { [($1, None)] }
;

elif  : ELIF expr NEWLINE stmts { Node.If ($2, $4, []) }
      | ELIF expr NEWLINE stmts ELSE stmts { Node.If ($2, $4, $6) }
      | ELIF expr NEWLINE stmts elif { Node.If ($2, $4, [$5]) }
;

names : NAME { [$1] }
      | names COMMA NAME { $1 @ [$3] }
;

patterns  : patterns pattern { $1 @ [$2] }
          | pattern { [$1] }
;

pattern : PATTERN { $1 }
;

expr  : assign_expr { $1 }
;

assign_expr : postfix_expr EQUAL conditional_expr {
  Node.Assign { Node.left=$1; Node.right=$3 }
}
            | conditional_expr { $1 }
;

conditional_expr  : logical_or_expr { $1 }
;

logical_or_expr : logical_and_expr { $1 }
;

logical_and_expr  : not_expr { $1 }
;

not_expr  : comparison { $1 }
;

comparison  : xor_expr LESS xor_expr {
  Node.Less { Node.left=$1; Node.right=$3 }
}
            | xor_expr LESS_EQUAL xor_expr {
  Node.LessEqual { Node.left=$1; Node.right=$3 }
}
            | xor_expr GREATER xor_expr {
  Node.Greater { Node.left=$1; Node.right=$3 }
}
            | xor_expr GREATER_EQUAL xor_expr {
  Node.GreaterEqual { Node.left=$1; Node.right=$3 }
}
            | xor_expr EQUAL_EQUAL xor_expr {
  Node.EqualEqual { Node.left=$1; Node.right=$3 }
}
            | xor_expr NOT_EQUAL xor_expr {
  Node.NotEqual { Node.left=$1; Node.right=$3 }
}
            | xor_expr { $1 }
;

xor_expr  : or_expr { $1 }
;

or_expr : and_expr { $1 }
;

and_expr  : shift_expr { $1 }
;

shift_expr  : arith_expr { $1 }
;

arith_expr  : arith_expr PLUS term { Node.Add { Node.left=$1; Node.right=$3 } }
            | arith_expr MINUS term { Node.Sub { Node.left=$1; Node.right=$3 } }
            | term { $1 }
;

term  : term STAR factor { Node.Mul { Node.left=$1; Node.right=$3 } }
      | term DIV factor { Node.Div { Node.left=$1; Node.right=$3 } }
      | term DIV_DIV factor { Node.DivDiv { Node.left=$1; Node.right=$3 } }
      | factor { $1 }
;

factor  : power { $1 }
;

power : postfix_expr { $1 }
;

postfix_expr  : postfix_expr LPAR exprs RPAR {
  Node.Call { Node.callee=$1; Node.args=$3 }
}
              | postfix_expr LPAR RPAR {
  Node.Call { Node.callee=$1; Node.args=[] }
}
              | postfix_expr LBRACKET expr RBRACKET {
  Node.Subscript { Node.prefix=$1; Node.index=$3 }
}
              | postfix_expr DOT NAME {
  Node.Attr { Node.attr_prefix=$1; Node.attr_name=$3 }
}
              | atom { $1 }
;

exprs : exprs COMMA expr { $1 @ [$3] }
      | expr { [$1] }
;

atom  : TRUE { Node.Const (Value.Bool true) }
      | FALSE { Node.Const (Value.Bool false) }
      | INT { Node.Const (Value.Int $1) }
      | FLOAT { Node.Const (Value.Float $1) }
      | STRING { Node.Const (Value.String $1) }
      | HEREDOC { Node.Heredoc $1 }
      | LBRACKET exprs RBRACKET { Node.Array $2 }
      | LBRACKET RBRACKET { Node.Array [] }
      | LBRACE pairs RBRACE { Node.Dict $2 }
      | LBRACE RBRACE { Node.Dict [] }
      | NAME { Node.Var $1 }
;

pairs : pair { [$1] }
      | pairs COMMA pair { $1 @ [$3] }
;

pair  : expr COLON expr { { Node.key=$1; Node.value=$3 } }
;
/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 */
