
let compile_node = function
    Node.Char c -> Op.Char c
  | Node.Star -> Op.Star
  | Node.StarStar -> Op.StarStar
  | _ -> failwith "Invalid node"

let compile_first_star = function
    Node.Star :: _ -> [Op.NotDot]
  | _ -> []

let compile nodes = (compile_first_star nodes) @ (List.map compile_node nodes)

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
