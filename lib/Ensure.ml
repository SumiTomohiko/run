
let ensure body cleanup =
  let retval = try
    body ()
  with
    e -> (cleanup (); raise e) in
  cleanup ();
  retval

let open_in path f =
  let ch = Pervasives.open_in path in
  ensure (fun () -> f ch) (fun () -> close_in ch)

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
