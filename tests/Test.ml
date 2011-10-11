
type t = {
  src: string;
  params: string option;
  out: string option;
  err: string option;
  stat: int option }

let make src params out err stat = {
  src=src;
  params=params;
  out=out;
  err=err;
  stat=stat }
let src_of_test test = test.src
let params_of_test test = test.params
let out_of_test test = test.out
let err_of_test test = test.err
let stat_of_test test = test.stat

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
