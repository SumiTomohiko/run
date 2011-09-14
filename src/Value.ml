
type t =
    Nil
  | Bool of bool
  | Int of Num.num
  | Float of float
  | String of string
  | Array of t array
  | Dict of (t, t) Hashtbl.t
  | Function of (t list -> t)
  | Method of t * (t -> t list -> t)
  (**
   * FIXME: Ops! True definition of UserFunction is "string list * Op.t", but
   * this causes mutual recursion! For avoiding this, second type is int, which
   * indicates index of Op.user_function_ops.
   *)
  | UserFunction of string list * int

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
