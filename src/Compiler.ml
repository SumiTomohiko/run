
let rec compile_expr = function
    Node.Assign { Node.left; Node.right } -> compile_expr right
  | Node.Call { Node.callee; Node.args } ->
      let op = Operation.Call (List.length args) in
      (compile_expr callee) @ (compile_exprs args) @ [op]
  | Node.Const (v) -> [Operation.PushConst v]
  | Node.Var (name) -> [Operation.PushLocal name]
and compile_exprs = function
    expr :: exprs -> (compile_expr expr) @ (compile_exprs exprs)
  | [] -> []

let compile_stmt = function
    Node.Expr (expr) -> (compile_expr expr) @ [Operation.Pop]

let rec compile = function
    stmt :: stmts -> (compile_stmt stmt) @ (compile stmts)
  | [] -> []

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
