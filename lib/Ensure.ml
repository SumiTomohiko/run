
let open_in path f =
  let ch = Pervasives.open_in path in
  Std.finally (fun () -> close_in ch) (fun () -> f ch) ()

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
