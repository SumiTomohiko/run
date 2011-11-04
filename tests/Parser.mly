%{
let trim = ExtString.String.strip
%}
%token <string> CONTENT
%token EOF ERR EXCEPTION FILENAME OUT PARAMS SRC STAT
%start test
%type <Test.t> test
%%
test
  : src filename_opt params_opt out_opt err_opt exception_opt stat_opt EOF {
    Test.make $1 $2 $3 $4 $5 $6 $7
  }
  ;

src
  : SRC contents { $2 }
  ;

filename_opt
  : { None }
  | FILENAME CONTENT { Some (String.sub $2 0 ((String.length $2) - 1)) }
  ;

params_opt
  : { None }
  | PARAMS CONTENT { Some (trim $2) }
  ;

out_opt
  : { None }
  | OUT contents { Some ($2) }
  ;

err_opt
  : { None }
  | ERR contents { Some $2 }
  ;

exception_opt
  : { None }
  | EXCEPTION CONTENT { Some (trim $2) }
  ;

stat_opt
  : { None }
  | STAT CONTENT { Some (int_of_string (trim $2)) }
  ;

contents
  : { "" }
  | contents CONTENT { $1 ^ $2 }
  ;

/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 */
