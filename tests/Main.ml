
let input_all ch s =
  try
    s ^ (input_line ch) ^ "\n"
  with
    End_of_file -> s

let main path =
  let inch = open_in path in
  let lexbuf = Lexing.from_channel inch in
  let test = Parser.test Lexer.token lexbuf in
  let _ = close_in inch in
  let (tmppath, outch) = Filename.open_temp_file "test_run." ".run" in
  let _ = output_string outch (Test.src_of_test test) in
  let _ = Unix.unlink tmppath in
  let (stdout, stdin) = Unix.open_process ("../src/run " ^ tmppath) in
  let actual = input_all stdout "" in
  let _ = Unix.close_process (stdout, stdin) in
  assert (actual = (Test.out_of_test test))

let _ = main (Array.get Sys.argv 1)

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
