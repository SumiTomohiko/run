%token AS COLON COMMA END EOF EQUAL EVERY FALSE LPAR MINUS NEWLINE PLUS RPAR
%token STAR TRUE
%token <Num.num> INT
%token <string> NAME PATTERN
%start script
%type <Node.stmt list> script
%%
script  : stmts EOF { $1 }
;
stmts : stmts stmt NEWLINE { $1 @ [$2] }
      | stmt NEWLINE { [$1] }
;
stmt  : expr { Node.Expr $1 }
      | EVERY patterns AS NAME NEWLINE stmts END {
  Node.Every { Node.patterns=$2; Node.name=$4; Node.stmts=$6 }
}
       | patterns { Node.Command $1 }
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
      | factor { $1 }
;
factor  : power { $1 }
;
power : postfix_expr { $1 }
;
postfix_expr  : atom { $1 }
              | postfix_expr LPAR exprs RPAR {
  Node.Call { Node.callee=$1; Node.args=$3 }
}
;
exprs : exprs COMMA expr { $1 @ [$3] }
      | expr { [$1] }
;
atom  : TRUE { Node.Const (Value.Bool (true)) }
      | FALSE { Node.Const (Value.Bool (false)) }
      | INT { Node.Const (Value.Int ($1)) }
      | NAME { Node.Var ($1) }
;
/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=sml
 */
