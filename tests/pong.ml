
let rec pong_ping = function
  | 0 -> ()
  | n ->
      let s = read_line () in
        assert (s = "PING");
        prerr_endline s;
        print_endline "PONG";
        pong_ping (n - 1)

let main () =
  let n = try
    int_of_string (Array.get Sys.argv 1)
  with
    Invalid_argument _ -> 1 in
  pong_ping n

let _ = main ()

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
