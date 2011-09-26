
type inline_nodes = InlineNode.t list

type t =
    BulletItem of int * inline_nodes
  | Paragraph of inline_nodes
  | Title of int * inline_nodes

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
