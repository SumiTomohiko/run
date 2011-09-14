%token AS COLON COMMA DIV DIV_DIV DOT END EOF EQUAL EVERY FALSE LBRACE LBRACKET
%token LPAR MINUS NEWLINE PLUS RBRACE RBRACKET RPAR STAR TRUE
%token <Num.num> INT
%token <float> FLOAT
%token <string> NAME PATTERN STRING
%start script
%type <Node.stmt list> script
%%
script  : stmts EOF { $1 }
;
stmts : stmts stmt NEWLINE { $1 @ [$2] }
      | stmt NEWLINE { [$1] }
;
stmt  : expr { Node.Expr $1 }
      | EVERY patterns AS names NEWLINE stmts END {
  Node.Every { Node.patterns=$2; Node.names=$4; Node.stmts=$6 }
}
       | patterns { Node.Command $1 }
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
comparison  : xor_expr { $1 }
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
term  : term STAR factor { Node.Mul { Node.left=$1; Node.right=$3 }}
      | term DIV factor { Node.Div { Node.left=$1; Node.right=$3 }}
      | term DIV_DIV factor { Node.DivDiv { Node.left=$1; Node.right=$3 }}
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
      | LBRACKET exprs RBRACKET { Node.Array $2 }
      | LBRACKET RBRACKET { Node.Array [] }
      | LBRACE pairs RBRACE { Node.Dict $2 }
      | LBRACE RBRACE { Node.Dict [] }
      | NAME { Node.Var ($1) }
;
pairs : pair { [$1] }
      | pairs COMMA pair { $1 @ [$3] }
;
pair  : expr COLON expr { { Node.key=$1; Node.value=$3 } }
;
/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=sml
 */
