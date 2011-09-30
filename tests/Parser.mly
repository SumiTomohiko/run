%token <string> CONTENT
%token EOF ERR OUT SRC STAT
%start test
%type <Test.t> test
%%
test
  : src out_opt err_opt stat_opt EOF { Test.make $1 $2 $3 $4 }
  ;

src
  : SRC contents { $2 }
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
