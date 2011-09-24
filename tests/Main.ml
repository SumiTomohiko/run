
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
  let cmd = "../src/run " ^ path in
  let stdout, stdin, stderr = Unix.open_process_full cmd [||] in
  let out = input_all stdout "" in
  let err = input_all stderr "" in
  (match Unix.close_process_full (stdout, stdin, stderr) with
    Unix.WEXITED 0 -> ()
  | _ -> assert false);
  out, err

let write_src src f =
  open_temp_file (fun ch -> output_string ch src) f

let do_test expected actual =
  match expected with
    Some s -> assert (actual = s)
  | None -> ()

let main path =
  let test = parse_test path in
  let out, err = write_src (Test.src_of_test test) exec_run in
  do_test (Test.out_of_test test) out;
  do_test (Test.err_of_test test) err

let _ = main (Array.get Sys.argv 1)

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
