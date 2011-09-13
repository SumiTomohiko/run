
let rec compile_expr oplist = function
    Node.Add (operands) -> compile_binop oplist operands Op.Add
  | Node.Array exprs ->
      (compile_exprs oplist exprs;
      OpList.add oplist (Op.MakeArray (List.length exprs)))
  | Node.Assign { Node.left; Node.right } ->
      (compile_expr oplist right;
      match left with
        Node.Subscript { Node.prefix; Node.index } ->
          (compile_expr oplist prefix;
          compile_expr oplist index;
          OpList.add oplist Op.StoreSubscript)
      | Node.Var (name) ->
          (OpList.add oplist (Op.StoreLocal name);
          OpList.add oplist (Op.PushLocal name))
      | _ -> raise (Failure "Unsupported assign expression"))
  | Node.Call { Node.callee; Node.args } ->
      (compile_expr oplist callee;
      compile_exprs oplist args;
      OpList.add oplist (Op.Call (List.length args)))
  | Node.Const (v) -> OpList.add oplist (Op.PushConst v)
  | Node.Div (operands) -> compile_binop oplist operands Op.Div
  | Node.DivDiv (operands) -> compile_binop oplist operands Op.DivDiv
  | Node.Mul (operands) -> compile_binop oplist operands Op.Mul
  | Node.Sub (operands) -> compile_binop oplist operands Op.Sub
  | Node.Subscript { Node.prefix; Node.index } ->
      (compile_expr oplist prefix;
      compile_expr oplist index;
      OpList.add oplist Op.Subscript)
  | Node.Var (name) -> OpList.add oplist (Op.PushLocal name)
and compile_binop oplist { Node.left; Node.right } op =
  compile_expr oplist left;
  compile_expr oplist right;
  OpList.add oplist op
and compile_exprs oplist = function
    expr :: exprs -> (compile_expr oplist expr; compile_exprs oplist exprs)
  | [] -> ()

let rec compile_every oplist { Node.patterns; Node.name; Node.stmts } =
  OpList.add oplist (Op.PushConst (Value.Bool false));
  let f pattern _ =
    OpList.add oplist (Op.PushConst (Value.String pattern));
    OpList.add oplist Op.Expand in
  List.fold_right f patterns ();
  let last = Op.make_label () in
  let top = Op.make_label () in
  OpList.add_op oplist top;
  OpList.add oplist (Op.JumpIfFalse last);
  OpList.add oplist (Op.StoreLocal name);
  compile_stmts oplist stmts;
  OpList.add oplist (Op.Jump top);
  OpList.add_op oplist last
and compile_stmt oplist = function
    Node.Command (patterns) ->
      let f _ pat = OpList.add oplist (Op.PushConst (Value.String pat)) in
      (List.fold_left f () patterns;
      OpList.add oplist (Op.Exec (List.length patterns)))
  | Node.Expr (expr) -> (compile_expr oplist expr; OpList.add oplist Op.Pop)
  | Node.Every (every) -> compile_every oplist every
and compile_stmts oplist = function
    hd :: tl -> (compile_stmt oplist hd; compile_stmts oplist tl)
  | [] -> ()

let compile stmts =
  let oplist = OpList.make () in
    compile_stmts oplist stmts;
    OpList.top oplist

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
