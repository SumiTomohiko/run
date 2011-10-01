
let compile_node = function
    Node.Char c -> Op.Char c
  | Node.Dir -> Op.Dir
  | Node.Star -> Op.Star
  | Node.StarStar -> Op.StarStar
  | _ -> failwith "Invalid node"

let compile_first_node = function
  | Node.Star -> [Op.NotDot]
  | _ -> []

let compile nodes =
  (compile_first_node (List.hd nodes)) @ (List.map compile_node nodes)

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
