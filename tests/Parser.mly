%token <string> CONTENT
%token EOF OUT SRC
%start test
%type <Test.t> test
%type <string> contents src
%type <string option> out
%%
test  : src out EOF { Test.make $1 $2 (*{ src=$1; out=$2 }*) }
;
src : SRC contents { $2 }
;
out : { None }
    | OUT contents { Some ($2) }
;
contents  : { "" }
          | contents CONTENT { $1 ^ $2 }
;
/**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=sml
 */
