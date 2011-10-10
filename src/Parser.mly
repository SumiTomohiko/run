%{
let make_default_return stmts =
  match (List.hd (List.rev stmts)) with
    Node.Return _ -> []
  | _ -> [Node.Return (Node.Const Value.Nil)]

let make_file_redirect path flags = Some (Node.File (path, flags))
let write_flags = [Unix.O_CREAT; Unix.O_WRONLY]
let make_write_redirect path =
  make_file_redirect path (Unix.O_TRUNC :: write_flags)
let make_append_redirect path =
  make_file_redirect path (Unix.O_APPEND :: write_flags)

let stderr_redirect = Some Node.Dup
%}
%token AS BAR BREAK COLON COMMA DEF DIV DIV_DIV DOLLER_LBRACE DOLLER_LPAR
%token DOLLER_QUESTION DOT ELIF ELSE END EOF EQUAL EQUAL_EQUAL ERR_RIGHT_ARROW
%token ERR_RIGHT_ARROW_OUT ERR_RIGHT_RIGHT_ARROW EVERY FALSE GREATER GREATER2
%token GREATER_EQUAL IF LBRACE LBRACKET LEFT_RIGHT_ARROW LESS LESS_EQUAL LPAR
%token MINUS NEWLINE NEXT NOT_EQUAL OUT_RIGHT_ARROW OUT_RIGHT_ARROW_ERR
%token OUT_RIGHT_RIGHT_ARROW PLUS RBRACE RBRACKET RETURN RIGHT_ARROW2
%token RIGHT_RIGHT_ARROW2 RPAR STAR TRUE WHILE
%token <Num.num> INT
%token <float> FLOAT
%token <string> NAME STRING
%token <Buffer.t> HEREDOC
%token <Matching.Main.t list> MATCHING_PATTERN
%start program
%type <Node.stmt list> program
%%
program
  : stmts EOF { $1 }
  ;

stmts
  : stmts NEWLINE stmt { $1 @ [$3] }
  | stmt { [$1] }
  ;

stmt
  : assign_expr { Node.Expr $1 }
  | call_expr { Node.Expr $1 }
  | DEF NAME LPAR names RPAR stmts END {
    let stmts = $6 @ (make_default_return $6) in
    Node.UserFunction { Node.uf_name=$2; Node.uf_args=$4; Node.uf_stmts=stmts }
  }
  | DEF NAME LPAR RPAR stmts END {
    let stmts = $5 @ (make_default_return $5) in
    Node.UserFunction { Node.uf_name=$2; Node.uf_args=[]; Node.uf_stmts=stmts }
  }
  | EVERY params AS names stmts END {
    Node.Every { Node.params=$2; Node.names=$4; Node.stmts=$5 }
  }
  | IF expr NEWLINE stmts END { Node.If ($2, $4, []) }
  | IF expr NEWLINE stmts ELSE stmts END { Node.If ($2, $4, $6) }
  | IF expr NEWLINE stmts elif END { Node.If ($2, $4, [$5]) }
  | WHILE expr NEWLINE stmts END { Node.While ($2, $4) }
  | NEXT { Node.Next }
  | BREAK { Node.Break }
  | RETURN expr { Node.Return $2 }
  | pipeline { Node.Pipeline $1 }
  | params LEFT_RIGHT_ARROW params { Node.Communication ($1, $3) }
  | /* empty */ { Node.Empty }
  ;

pipeline
  : single_command { [$1] }
  | first_command last_command { [$1; $2] }
  | first_command middle_commands last_command { $1 :: ($2 @ [$3]) }
  ;

single_command
  : params stdin_opt stderr_opt stdout_opt {
    ($1, $2, $4, $3)
  }
  | params stdin_opt RIGHT_ARROW2 redirect_dest {
    ($1, $2, make_write_redirect $4, stderr_redirect)
  }
  | params stdin_opt RIGHT_RIGHT_ARROW2 redirect_dest {
    ($1, $2, make_append_redirect $4, stderr_redirect)
  }
  ;

redirect_dest
  : MATCHING_PATTERN {
    match $1 with
      Matching.Main.Static path :: [] -> path
    | _ -> failwith "Sorry, redirect destination must be static string."
  }
  ;

stdin_opt
  : /* empty */ { None }
  | LESS redirect_dest { Some $2 }
  ;

stderr_opt
  : /* empty */ { None }
  | ERR_RIGHT_ARROW redirect_dest { make_write_redirect $2 }
  | ERR_RIGHT_RIGHT_ARROW redirect_dest { make_append_redirect $2 }
  | ERR_RIGHT_ARROW_OUT { stderr_redirect }
  ;

stdout_opt
  : /* empty */ { None }
  | GREATER redirect_dest { make_write_redirect $2 }
  | GREATER2 redirect_dest { make_append_redirect $2 }
  | OUT_RIGHT_ARROW_ERR { Some Node.Dup }
  | OUT_RIGHT_ARROW redirect_dest { make_write_redirect $2 }
  | OUT_RIGHT_RIGHT_ARROW redirect_dest { make_append_redirect $2 }
  ;

first_command
  : params stdin_opt stderr_opt BAR { ($1, $2, None, $3) }
  | params stdin_opt RIGHT_ARROW2 BAR { ($1, $2, None, stderr_redirect) }
  ;

last_command
  : params stderr_opt stdout_opt { ($1, None, $3, $2) }
  | params RIGHT_ARROW2 redirect_dest {
    ($1, None, make_write_redirect $3, stderr_redirect)
  }
  | params RIGHT_RIGHT_ARROW2 redirect_dest {
    ($1, None, make_append_redirect $3, stderr_redirect)
  }
  ;

middle_commands
  : middle_commands middle_command { $1 @ [$2] }
  | middle_command { [$1] }
  ;

middle_command
  : params stderr_opt BAR { ($1, None, None, $2) }
  | params RIGHT_ARROW2 BAR { ($1, None, None, stderr_redirect) }
  ;

elif
  : ELIF expr NEWLINE stmts { Node.If ($2, $4, []) }
  | ELIF expr NEWLINE stmts ELSE stmts { Node.If ($2, $4, $6) }
  | ELIF expr NEWLINE stmts elif { Node.If ($2, $4, [$5]) }
  ;

names
  : NAME { [$1] }
  | names COMMA NAME { $1 @ [$3] }
  ;

params
  : params param { $1 @ $2 }
  | param { $1 }
  ;

param
  : MATCHING_PATTERN { List.map (fun param -> Node.MatchingParam param) $1 }
  | DOLLER_LBRACE expr RBRACE { [Node.ExprParam $2] }
  ;

expr
  : assign_expr { $1 }
  | conditional_expr { $1 }
  ;

assign_expr
  : postfix_expr EQUAL conditional_expr {
    Node.Assign { Node.left=$1; Node.right=$3 }
  }
  ;

conditional_expr
  : logical_or_expr { $1 }
  ;

logical_or_expr
  : logical_and_expr { $1 }
  ;

logical_and_expr
  : not_expr { $1 }
  ;

not_expr
  : comparison { $1 }
  ;

comparison
  : xor_expr LESS xor_expr { Node.Less { Node.left=$1; Node.right=$3 } }
  | xor_expr LESS_EQUAL xor_expr {
    Node.LessEqual { Node.left=$1; Node.right=$3 }
  }
  | xor_expr GREATER xor_expr { Node.Greater { Node.left=$1; Node.right=$3 } }
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

xor_expr
  : or_expr { $1 }
  ;

or_expr
  : and_expr { $1 }
  ;

and_expr
  : shift_expr { $1 }
  ;

shift_expr
  : arith_expr { $1 }
  ;

arith_expr
  : arith_expr PLUS term { Node.Add { Node.left=$1; Node.right=$3 } }
  | arith_expr MINUS term { Node.Sub { Node.left=$1; Node.right=$3 } }
  | term { $1 }
  ;

term
  : term STAR factor { Node.Mul { Node.left=$1; Node.right=$3 } }
  | term DIV factor { Node.Div { Node.left=$1; Node.right=$3 } }
  | term DIV_DIV factor { Node.DivDiv { Node.left=$1; Node.right=$3 } }
  | factor { $1 }
  ;

factor
  : power { $1 }
  ;

power
  : postfix_expr { $1 }
  ;

call_expr
  : postfix_expr LPAR exprs RPAR { Node.Call { Node.callee=$1; Node.args=$3 } }
  | postfix_expr LPAR RPAR { Node.Call { Node.callee=$1; Node.args=[] } }
  ;

postfix_expr
  : call_expr { $1 }
  | postfix_expr LBRACKET expr RBRACKET {
    Node.Subscript { Node.prefix=$1; Node.index=$3 }
  }
  | postfix_expr DOT NAME {
    Node.Attr { Node.attr_prefix=$1; Node.attr_name=$3 }
  }
  | atom { $1 }
  ;

exprs
  : exprs COMMA expr { $1 @ [$3] }
  | expr { [$1] }
  ;

atom
  : TRUE { Node.Const (Value.Bool true) }
  | FALSE { Node.Const (Value.Bool false) }
  | INT { Node.Const (Value.Int $1) }
  | FLOAT { Node.Const (Value.Float $1) }
  | STRING { Node.Const (Value.String $1) }
  | HEREDOC { Node.Heredoc $1 }
  | LBRACKET exprs RBRACKET { Node.Array $2 }
  | LBRACKET RBRACKET { Node.Array [] }
  | LBRACE dict_pairs RBRACE { Node.Dict $2 }
  | LBRACE RBRACE { Node.Dict [] }
  | NAME { Node.Var $1 }
  | DOLLER_LPAR pipeline RPAR { Node.InlinePipeline $2 }
  | DOLLER_QUESTION { Node.LastStatus }
  ;

dict_pairs
  : dict_pair { [$1] }
  | dict_pairs COMMA dict_pair { $1 @ [$3] }
  ;

dict_pair
  : expr COLON expr { { Node.key=$1; Node.value=$3 } }
  ;

/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 */
