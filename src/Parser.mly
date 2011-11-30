%{
let make_node body pos = ((pos.Lexing.pos_fname, pos.Lexing.pos_lnum), body)
let make_const v = make_node (Node.Const v)
let make_array exprs = make_node (Node.Array exprs)
let make_dict pairs = make_node (Node.Dict pairs)
let make_call callee args =
  make_node (Node.Call { Node.callee=callee; Node.args=args })

let make_default_return stmts pos =
  match (snd (List.hd (List.rev stmts))) with
    Node.Return _ -> []
  | _ -> [make_node (Node.Return (make_const Value.Nil pos)) pos]

let make_file_redirect path flags = Some (Node.File (path, flags))
let write_flags = [Unix.O_CREAT; Unix.O_WRONLY]
let make_write_redirect path =
  make_file_redirect path (Unix.O_TRUNC :: write_flags)
let make_append_redirect path =
  make_file_redirect path (Unix.O_APPEND :: write_flags)

let stderr_redirect = Some Node.Dup

let rec nodes_of_string result s index =
  if (String.length s) = index then
    result
  else
    nodes_of_string (result @ [Node.Char (String.get s index)]) s (index + 1)
%}
%token AS BAR BREAK COLON COMMA DEF DIV DIV_DIV DOLLER_LBRACE DOLLER_LPAR
%token DOLLER_QUESTION DOT DOUBLE_QUOTE EXCEPT ELIF ELSE END EOF EQUAL
%token EQUAL_EQUAL ERR_RIGHT_ARROW ERR_RIGHT_ARROW_OUT ERR_RIGHT_RIGHT_ARROW
%token EVERY FALSE FINALLY GREATER GREATER_GREATER GREATER_EQUAL IF ITERATE
%token LBRACE LBRACKET LEFT_RIGHT_ARROW LESS LESS_EQUAL LPAR MINUS NEWLINE NEXT
%token NOT_EQUAL OUT_RIGHT_ARROW OUT_RIGHT_ARROW_ERR OUT_RIGHT_RIGHT_ARROW
%token PARAM_BEGIN PARAM_END PLUS RAISE RBRACE RBRACKET RETURN RIGHT_ARROW
%token RIGHT_RIGHT_ARROW RPAR SEP STAR STAR_STAR TRUE TRY WHILE
%token <char> CHAR
%token <Num.num> INT
%token <float> FLOAT
%token <string> NAME STRING
%token <Buffer.t> HEREDOC
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
  : assign_expr { make_node (Node.Expr $1) $startpos($1) }
  | call_expr { make_node (Node.Expr $1) $startpos($1) }
  | DEF NAME LPAR names RPAR stmts END {
    let stmts = $6 @ (make_default_return $6 $startpos($7)) in
    make_node (Node.UserFunction {
      Node.uf_name=$2;
      Node.uf_args=$4;
      Node.uf_stmts=stmts }) $startpos($1)
  }
  | DEF NAME LPAR RPAR stmts END {
    let stmts = $5 @ (make_default_return $5 $startpos($6)) in
    make_node (Node.UserFunction {
      Node.uf_name=$2;
      Node.uf_args=[];
      Node.uf_stmts=stmts }) $startpos($1)
  }
  | EVERY params AS names stmts END {
    let body = Node.Every { Node.params=$2; Node.names=$4; Node.stmts=$5 } in
    make_node body $startpos($1)
  }
  | ITERATE expr AS names stmts END {
    make_node (Node.Iterate ($2, $4, $5)) $startpos($1)
  }
  | IF expr NEWLINE stmts END { make_node (Node.If ($2, $4, [])) $startpos($1) }
  | IF expr NEWLINE stmts ELSE stmts END {
    make_node (Node.If ($2, $4, $6)) $startpos($1)
  }
  | IF expr NEWLINE stmts elif END {
    make_node (Node.If ($2, $4, [$5])) $startpos($1)
  }
  | WHILE expr NEWLINE stmts END {
    make_node (Node.While ($2, $4)) $startpos($1)
  }
  | NEXT { make_node Node.Next $startpos($1) }
  | BREAK { make_node Node.Break $startpos($1) }
  | RETURN expr { make_node (Node.Return $2) $startpos($1) }
  | TRY stmts excepts END { make_node (Node.Try ($2, $3, [])) $startpos($1) }
  | TRY stmts FINALLY stmts END {
    make_node (Node.Try ($2, [], $4)) $startpos($1)
  }
  | TRY stmts excepts FINALLY stmts END {
    make_node (Node.Try ($2, $3, $5)) $startpos($1)
  }
  | RAISE expr { make_node (Node.Raise (Some $2)) $startpos($1) }
  | RAISE { make_node (Node.Raise None) $startpos($1) }
  | pipeline { make_node (Node.Pipeline $1) $startpos($1) }
  | params LEFT_RIGHT_ARROW params {
    make_node (Node.Communication ($1, $3)) $startpos($1)
  }
  | /* empty */ { (Node.dummy_pos, Node.Empty) }
  ;

excepts
  : excepts except { $1 @ [$2] }
  | except { [$1] }
  ;

except
  : EXCEPT exprs as_opt NEWLINE stmts { ($2, $3, $5) }
  ;

as_opt
  : /* empty */ { None }
  | AS NAME { Some $2 }
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
  | params stdin_opt RIGHT_ARROW redirect_dest {
    ($1, $2, make_write_redirect $4, stderr_redirect)
  }
  | params stdin_opt RIGHT_RIGHT_ARROW redirect_dest {
    ($1, $2, make_append_redirect $4, stderr_redirect)
  }
  ;

redirect_dest
  : PARAM_BEGIN param_body PARAM_END {
    let s, l = Param.chain_static_chars "" $2 in
    assert ((List.length l) = 0);
    s
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
  | GREATER_GREATER redirect_dest { make_append_redirect $2 }
  | OUT_RIGHT_ARROW_ERR { Some Node.Dup }
  | OUT_RIGHT_ARROW redirect_dest { make_write_redirect $2 }
  | OUT_RIGHT_RIGHT_ARROW redirect_dest { make_append_redirect $2 }
  ;

first_command
  : params stdin_opt stderr_opt BAR { ($1, $2, None, $3) }
  | params stdin_opt RIGHT_ARROW BAR { ($1, $2, None, stderr_redirect) }
  ;

last_command
  : params stderr_opt stdout_opt { ($1, None, $3, $2) }
  | params RIGHT_ARROW redirect_dest {
    ($1, None, make_write_redirect $3, stderr_redirect)
  }
  | params RIGHT_RIGHT_ARROW redirect_dest {
    ($1, None, make_append_redirect $3, stderr_redirect)
  }
  ;

middle_commands
  : middle_commands middle_command { $1 @ [$2] }
  | middle_command { [$1] }
  ;

middle_command
  : params stderr_opt BAR { ($1, None, None, $2) }
  | params RIGHT_ARROW BAR { ($1, None, None, stderr_redirect) }
  ;

elif
  : ELIF expr NEWLINE stmts { make_node (Node.If ($2, $4, [])) $startpos($1) }
  | ELIF expr NEWLINE stmts ELSE stmts {
    make_node (Node.If ($2, $4, $6)) $startpos($1)
  }
  | ELIF expr NEWLINE stmts elif {
    make_node (Node.If ($2, $4, [$5])) $startpos($1)
  }
  ;

names
  : NAME { [$1] }
  | names COMMA NAME { $1 @ [$3] }
  ;

params
  : params param { $1 @ [$2] }
  | param { [$1] }
  ;

param
  : PARAM_BEGIN param_body PARAM_END { $2 }
  | PARAM_BEGIN STRING PARAM_END { nodes_of_string [] $2 0 }
  ;

param_body
  : param_body param_atom { $1 @ [$2] }
  | param_atom { [$1] }
  ;

param_atom
  : CHAR { Node.Char $1 }
  | SEP { Node.Dir }
  | STAR { Node.Star }
  | STAR_STAR { Node.StarStar }
  | LBRACE param_bodies RBRACE { Node.Branch $2 }
  | DOLLER_LBRACE expr RBRACE {
    Node.ExprParam { Node.ep_index=0; Node.ep_expr=$2 }
  }
  ;

param_bodies
  : param_bodies COMMA param_body { $1 @ [$3] }
  | param_body { [$1] }
  ;

expr
  : assign_expr { $1 }
  | conditional_expr { $1 }
  ;

assign_expr
  : postfix_expr EQUAL conditional_expr {
    make_node (Node.Assign { Node.left=$1; Node.right=$3 }) $startpos($1)
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
  : xor_expr LESS xor_expr {
    make_node (Node.Less { Node.left=$1; Node.right=$3 }) $startpos($1)
  }
  | xor_expr LESS_EQUAL xor_expr {
    make_node (Node.LessEqual { Node.left=$1; Node.right=$3 }) $startpos($1)
  }
  | xor_expr GREATER xor_expr {
    make_node (Node.Greater { Node.left=$1; Node.right=$3 }) $startpos($1)
  }
  | xor_expr GREATER_EQUAL xor_expr {
    make_node (Node.GreaterEqual { Node.left=$1; Node.right=$3 }) $startpos($1)
  }
  | xor_expr EQUAL_EQUAL xor_expr {
    make_node (Node.EqualEqual { Node.left=$1; Node.right=$3 }) $startpos($1)
  }
  | xor_expr NOT_EQUAL xor_expr {
    make_node (Node.NotEqual { Node.left=$1; Node.right=$3 }) $startpos($1)
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
  : arith_expr PLUS term {
    make_node (Node.Add { Node.left=$1; Node.right=$3 }) $startpos($1)
  }
  | arith_expr MINUS term {
    make_node (Node.Sub { Node.left=$1; Node.right=$3 }) $startpos($1)
  }
  | term { $1 }
  ;

term
  : term STAR factor {
    make_node (Node.Mul { Node.left=$1; Node.right=$3 }) $startpos($1)
  }
  | term DIV factor {
    make_node (Node.Div { Node.left=$1; Node.right=$3 }) $startpos($1)
  }
  | term DIV_DIV factor {
    make_node (Node.DivDiv { Node.left=$1; Node.right=$3 }) $startpos($1)
  }
  | factor { $1 }
  ;

factor
  : power { $1 }
  ;

power
  : postfix_expr { $1 }
  ;

call_expr
  : postfix_expr LPAR exprs RPAR { make_call $1 $3 $startpos($2) }
  | postfix_expr LPAR RPAR { make_call $1 [] $startpos($2) }
  ;

postfix_expr
  : call_expr { $1 }
  | postfix_expr LBRACKET expr RBRACKET {
    make_node (Node.Subscript { Node.prefix=$1; Node.index=$3 }) $startpos($1)
  }
  | postfix_expr DOT NAME {
    let pos = $startpos($1) in
    make_node (Node.Attr { Node.attr_prefix=$1; Node.attr_name=$3 }) pos
  }
  | atom { $1 }
  ;

exprs
  : exprs COMMA expr { $1 @ [$3] }
  | expr { [$1] }
  ;

atom
  : TRUE { make_const (Value.Bool true) $startpos($1) }
  | FALSE { make_const (Value.Bool false) $startpos($1) }
  | INT { make_const (Value.Int $1) $startpos($1) }
  | FLOAT { make_const (Value.Float $1) $startpos($1) }
  | DOUBLE_QUOTE string_contents_opt DOUBLE_QUOTE {
    make_node (Node.String $2) $startpos($1)
  }
  | HEREDOC { make_node (Node.Heredoc $1) $startpos($1) }
  | LBRACKET exprs RBRACKET { make_array $2 $startpos($1) }
  | LBRACKET RBRACKET { make_array [] $startpos($1) }
  | LBRACE dict_pairs RBRACE { make_dict $2 $startpos($1) }
  | LBRACE RBRACE { make_dict [] $startpos($1) }
  | NAME { make_node (Node.Var $1) $startpos($1) }
  | DOLLER_LPAR pipeline RPAR {
    make_node (Node.InlinePipeline $2) $startpos($1)
  }
  | DOLLER_QUESTION { make_node Node.LastStatus $startpos($1) }
  ;

string_contents_opt
  : /* empty */ { [] }
  | string_contents { $1 }
  ;

string_contents
  : string_contents string_content { $1 @ [$2] }
  | string_content { [$1] }
  ;

string_content
  : STRING { make_const (Value.String $1) $startpos($1) }
  | DOLLER_LBRACE expr RBRACE { $2 }
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
