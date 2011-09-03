
structure Try = struct
  fun try body finally =
  let
    val ret = (body ()) handle e => (finally (); raise e)
  in
    finally ();
    ret
  end
end

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
