%token EOF EQUAL FALSE TRUE
%token <Num.num> INT
%start script
%type <Node.t list> script
%%
script  : stmts EOF { $1 }
;
stmts : stmts stmt { $1 @ [$2] }
      | stmt { [$1] }
;
stmt  : expr { $1 }
;
expr  : assign_expr { $1 }
;
assign_expr : postfix_expr EQUAL conditional_expr {
  Node.AssignExpr ({ Node.left=$1; Node.right=$3 })
}
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
arith_expr  : term { $1 }
;
term  : factor { $1 }
;
factor  : power { $1 }
;
power : postfix_expr { $1 }
;
postfix_expr  : atom { $1 }
;
atom  : TRUE { Node.Atom (Value.Bool (true)) }
      | FALSE { Node.Atom (Value.Bool (false)) }
      | INT { Node.Atom (Value.Int ($1)) }
;
/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=sml
 */
