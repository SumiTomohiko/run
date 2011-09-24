
type t = {
  src: string;
  out: string option;
  err: string option;
  stat: int option }

let make src out err stat = { src=src; out=out; err=err; stat=stat }
let src_of_test test = test.src
let out_of_test test = test.out
let err_of_test test = test.err
let stat_of_test test = test.stat

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
