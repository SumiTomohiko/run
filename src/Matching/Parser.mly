%token FOO
%type<int> program
%start program
%%
program
  : { 42 }
  ;
%%
(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
