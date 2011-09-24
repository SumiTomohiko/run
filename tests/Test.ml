
type t = { src: string; out: string option; err: string option }

let make src out err = { src=src; out=out; err=err }
let src_of_test test = test.src
let out_of_test test = test.out
let err_of_test test = test.err

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
