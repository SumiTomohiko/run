%token <string> CONTENT
%token EOF ERR OUT SRC
%start test
%type <Test.t> test
%%
test  : src out_opt err_opt EOF { Test.make $1 $2 $3 }
;
src : SRC contents { $2 }
;
out_opt : { None }
        | OUT contents { Some ($2) }
;
err_opt : { None }
        | ERR contents { Some $2 }
;
contents  : { "" }
          | contents CONTENT { $1 ^ $2 }
;
/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=sml
 */
