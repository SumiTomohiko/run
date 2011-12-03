
let rec input_all ch s =
  try
    input_all ch (s ^ (input_line ch) ^ "\n")
  with
    End_of_file -> s

let parse_test path =
  Ensure.open_in path (fun ch ->
    Parser.test Lexer.token (Lexing.from_channel ch))

let open_temp_file filename f g =
  let path, ch = match filename with
  | Some s ->
      let path = Printf.sprintf "/tmp/%s" s in
      path, open_out path
  | None -> Filename.open_temp_file "test_run." ".run" in
  let close () = close_out ch in
  let body () =
    Std.finally close f ch;
    g path in
  Std.finally (fun () -> Unix.unlink path) body ()

let exec_run path params =
  let s = match params with
  | Some params -> [params]
  | None -> [] in
  let cmd = String.concat " " (["../src/run"; path] @ s) in
  let stdout, stdin, stderr = Unix.open_process_full cmd [||] in
  let out = input_all stdout "" in
  let err = input_all stderr "" in
  match Unix.close_process_full (stdout, stdin, stderr) with
  | Unix.WEXITED stat -> out, err, stat
  | _ -> assert false

let write_src src filename f =
  open_temp_file filename (fun ch -> output_string ch src) f

let do_test expected actual =
  match expected with
  | Some s -> assert (actual = s)
  | None -> ()

let do_exception_test test err =
  match Test.exception_of_test test with
  | Some _ as expected ->
      let split = ExtString.String.nsplit in
      do_test expected (List.hd (List.tl (List.rev (split err "\n"))))
  | None -> ()

let main path =
  let test = parse_test path in
  let f path = exec_run path (Test.params_of_test test) in
  let src = Test.src_of_test test in
  let out, err, stat = write_src src (Test.filename_of_test test) f in
  do_test (Test.out_of_test test) out;
  do_test (Test.err_of_test test) err;
  do_exception_test test err;
  match Test.stat_of_test test with
  | Some expected -> assert (expected = stat)
  | None -> ()

let _ = main (Array.get Sys.argv 1)

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
