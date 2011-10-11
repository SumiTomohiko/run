%token <string> CONTENT
%token EOF ERR OUT PARAMS SRC STAT
%start test
%type <Test.t> test
%%
test
  : src params_opt out_opt err_opt stat_opt EOF { Test.make $1 $2 $3 $4 $5 }
  ;

src
  : SRC contents { $2 }
  ;

params_opt
  : { None }
  | PARAMS contents {
    let s = $2 in
    Some (String.sub s 0 ((String.length s) - 1))
  }
  ;

out_opt
  : { None }
  | OUT contents { Some ($2) }
  ;

err_opt
  : { None }
  | ERR contents { Some $2 }
  ;

stat_opt
  : { None }
  | STAT contents {
    Some (int_of_string (String.sub $2 0 ((String.length $2) - 1)))
  }
  ;

contents
  : { "" }
  | contents CONTENT { $1 ^ $2 }
  ;

/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 */
