
let ensure = Ensure.ensure

let rec input_all ch s =
  try
    input_all ch (s ^ (input_line ch) ^ "\n")
  with
    End_of_file -> s

let parse_test path =
  Ensure.open_in path (fun ch ->
    Parser.test Lexer.token (Lexing.from_channel ch))

let open_temp_file f g =
  let (path, ch) = Filename.open_temp_file "test_run." ".run" in
  let body () = ((ensure (fun () -> f ch) (fun () -> close_out ch)); g path) in
  ensure body (fun () -> Unix.unlink path)

let exec_run path =
  let (stdout, stdin) = Unix.open_process ("../src/run " ^ path) in
  let actual = input_all stdout "" in
  let _ = Unix.close_process (stdout, stdin) in
  actual

let write_src src f =
  open_temp_file (fun ch -> output_string ch src) f

let main path =
  let test = parse_test path in
  let actual = write_src (Test.src_of_test test) exec_run in
  match (Test.out_of_test test) with
    Some (out) -> assert (actual = out)
  | _ -> ()

let _ = main (Array.get Sys.argv 1)

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
