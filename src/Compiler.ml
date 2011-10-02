
(* FIXME: This is global *)
let while_stack = Stack.create ()

let rec compile_params oplist =
  let f = function
      Node.ExprParam expr ->
        compile_expr oplist expr;
        OpList.add oplist Op.MoveParam
    | Node.MatchingParam m ->
        match m with
          Matching.Main.Static s ->
            OpList.add oplist (Op.PushConst (Value.String s));
            OpList.add oplist Op.MoveParam
        | Matching.Main.Dynamic pattern ->
            OpList.add oplist (Op.ExpandParam pattern) in
  List.iter f
and add_command oplist params path flags =
  OpList.add oplist (Op.PushConst path);
  OpList.add oplist (Op.PushCommand flags);
  compile_params oplist params
and compile_commands oplist = function
    (params, _, _, Some Node.Dup) :: tl ->
      OpList.add oplist Op.PushCommandE2O;
      compile_params oplist params;
      compile_commands oplist tl
  | (params, _, _, Some (Node.File (path, flags))) :: tl ->
      add_command oplist params (Value.String path) flags;
      compile_commands oplist tl
  | (params, _, _, None) :: tl ->
      add_command oplist params Value.Nil [];
      compile_commands oplist tl
  | [] -> ()
and compile_pipeline oplist commands last_op =
  let _, first_stdin, _, _ = List.hd commands in
  let stdin_path = match first_stdin with
    Some path -> Value.String path
  | None -> Value.Nil in
  OpList.add oplist (Op.PushConst stdin_path);
  let _, _, last_stdout, _ = List.hd (List.rev commands) in
  let stdout_path, flags = match last_stdout with
    Some (Node.File (path, flags)) -> Value.String path, flags
  | None -> Value.Nil, []
  | Some _ -> failwith "Invalid stdout redirect" in
  OpList.add oplist (Op.PushConst stdout_path);
  OpList.add oplist (Op.PushPipeline flags);
  compile_commands oplist commands;
  OpList.add oplist last_op
and compile_expr oplist = function
    Node.Add operands -> compile_binop oplist operands Op.Add
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
      | Node.Var name ->
          (OpList.add oplist (Op.StoreLocal name);
          OpList.add oplist (Op.PushLocal name))
      | _ -> failwith "Unsupported assign expression")
  | Node.Attr { Node.attr_prefix; Node.attr_name } ->
      (compile_expr oplist attr_prefix;
      OpList.add oplist (Op.GetAttr attr_name))
  | Node.Call { Node.callee; Node.args } ->
      (compile_expr oplist callee;
      compile_exprs oplist args;
      OpList.add oplist (Op.Call (List.length args)))
  | Node.Const v -> OpList.add oplist (Op.PushConst v)
  | Node.Dict pairs ->
      let compile_pair { Node.key; Node.value } =
        compile_expr oplist key;
        compile_expr oplist value in
      List.iter compile_pair pairs;
      OpList.add oplist (Op.MakeDict (List.length pairs))
  | Node.Div operands -> compile_binop oplist operands Op.Div
  | Node.DivDiv operands -> compile_binop oplist operands Op.DivDiv
  | Node.EqualEqual operands -> compile_binop oplist operands Op.Equal
  | Node.Greater operands -> compile_binop oplist operands Op.Greater
  | Node.GreaterEqual operands -> compile_binop oplist operands Op.GreaterEqual
  | Node.Heredoc buf ->
      let s = Buffer.contents buf in
      OpList.add oplist (Op.PushConst (Value.String s))
  | Node.InlinePipeline commands ->
      compile_pipeline oplist commands Op.ExecAndPush
  | Node.LastStatus -> OpList.add oplist Op.PushLastStatus
  | Node.Less operands -> compile_binop oplist operands Op.Less
  | Node.LessEqual operands -> compile_binop oplist operands Op.LessEqual
  | Node.Mul operands -> compile_binop oplist operands Op.Mul
  | Node.NotEqual operands -> compile_binop oplist operands Op.NotEqual
  | Node.Sub operands -> compile_binop oplist operands Op.Sub
  | Node.Subscript { Node.prefix; Node.index } ->
      compile_expr oplist prefix;
      compile_expr oplist index;
      OpList.add oplist Op.Subscript
  | Node.Var name -> OpList.add oplist (Op.PushLocal name)
and compile_binop oplist { Node.left; Node.right } op =
  compile_expr oplist left;
  compile_expr oplist right;
  OpList.add oplist op
and compile_exprs oplist = function
    expr :: exprs -> (compile_expr oplist expr; compile_exprs oplist exprs)
  | [] -> ()

let rec compile_every oplist { Node.params; Node.names; Node.stmts } =
  let push_false _ = OpList.add oplist (Op.PushConst (Value.Bool false)) in
  List.iter push_false names;
  let f = function
    Node.ExprParam expr -> compile_expr oplist expr
  | Node.MatchingParam m ->
      let op = match m with
        Matching.Main.Static s -> Op.PushConst (Value.String s)
      | Matching.Main.Dynamic pat -> Op.Expand pat in
      OpList.add oplist op in
  List.iter f (List.rev params);
  let last = Op.make_label () in
  let top = Op.make_label () in
  OpList.add_op oplist top;
  OpList.add oplist (Op.JumpIfFalse last);
  let store name = OpList.add oplist (Op.StoreLocal name) in
  List.iter store names;
  compile_stmts oplist stmts;
  OpList.add oplist (Op.Jump top);
  OpList.add_op oplist last
and compile_stmt oplist = function
    Node.Break ->
      let _, label = Stack.top while_stack in
      OpList.add oplist (Op.Jump label)
  | Node.Communication (left, right) ->
      OpList.add oplist (Op.PushConst Value.Nil);
      OpList.add oplist (Op.PushConst Value.Nil);
      OpList.add oplist (Op.PushPipeline []);
      OpList.add oplist (Op.PushConst Value.Nil);
      OpList.add oplist (Op.PushCommand []);
      compile_params oplist left;
      OpList.add oplist (Op.PushConst Value.Nil);
      OpList.add oplist (Op.PushCommand []);
      compile_params oplist right;
      OpList.add oplist Op.Communicate
  | Node.Expr expr ->
      compile_expr oplist expr;
      OpList.add oplist Op.Pop
  | Node.Every every -> compile_every oplist every
  | Node.If (expr, stmts1, stmts2) ->
      let else_begin = Op.make_label () in
      let else_end = Op.make_label () in
      compile_expr oplist expr;
      OpList.add oplist (Op.JumpIfFalse else_begin);
      compile_stmts oplist stmts1;
      OpList.add oplist (Op.Jump else_end);
      OpList.add_op oplist else_begin;
      compile_stmts oplist stmts2;
      OpList.add_op oplist else_end
  | Node.Next ->
      let label, _ = Stack.top while_stack in
      OpList.add oplist (Op.Jump label)
  | Node.Pipeline commands -> compile_pipeline oplist commands Op.Exec
  | Node.Return expr ->
      compile_expr oplist expr;
      OpList.add oplist Op.Return
  | Node.UserFunction { Node.uf_name; Node.uf_args; Node.uf_stmts } ->
      let func_ops = OpList.make () in
      compile_stmts func_ops uf_stmts;
      let func_ops_index = Op.register_ops (OpList.top func_ops) in
      let op = Op.MakeUserFunction (uf_args, func_ops_index) in
      OpList.add oplist op;
      OpList.add oplist (Op.StoreLocal uf_name)
  | Node.While (expr, stmts) ->
      let while_begin = Op.make_label () in
      let while_end = Op.make_label () in
      Stack.push (while_begin, while_end) while_stack;
      OpList.add_op oplist while_begin;
      compile_expr oplist expr;
      OpList.add oplist (Op.JumpIfFalse while_end);
      compile_stmts oplist stmts;
      OpList.add oplist (Op.Jump while_begin);
      OpList.add_op oplist while_end;
      ignore (Stack.pop while_stack)
  | Node.Empty -> ()
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
