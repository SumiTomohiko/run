
type t = {
  src: string;
  filename: string option;
  params: string option;
  out: string option;
  err: string option;
  exc: string option;
  stat: int option }

let make src filename params out err exc stat = {
  src=src;
  filename=filename;
  params=params;
  out=out;
  err=err;
  exc=exc;
  stat=stat }
let src_of_test test = test.src
let filename_of_test test = test.filename
let params_of_test test = test.params
let out_of_test test = test.out
let err_of_test test = test.err
let exception_of_test test = test.exc
let stat_of_test test = test.stat

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
