type t = { src: string; out: string option }

let make src out = { src=src; out=out }
let src_of_test test = test.src
let out_of_test test = test.src

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
