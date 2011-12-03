
let rec ping_pong = function
  | 0 -> ()
  | n ->
    print_endline "PING";
    let s = read_line () in
      assert (s = "PONG");
      prerr_endline s;
      ping_pong (n - 1)

let main () =
  let n = try
    int_of_string (Array.get Sys.argv 1)
  with
  | Invalid_argument _ -> 1 in
  ping_pong n

let _ = main ()

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
