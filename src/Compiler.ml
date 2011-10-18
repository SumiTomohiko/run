
(* FIXME: This is global *)
let while_stack = Stack.create ()

let rec find_expr_param founds = function
  | (Node.Branch patterns) :: tl ->
      let exprs = List.fold_left find_expr_param founds patterns in
      find_expr_param exprs tl
  | (Node.ExprParam expr) :: tl -> find_expr_param (founds @ [expr]) tl
  | _ :: tl -> find_expr_param founds tl
  | [] -> founds

let rec number_expr_params num = function
  | hd :: tl ->
      hd.Node.ep_index <- num;
      number_expr_params (num + 1) tl
  | [] -> ()

let rec expand_branch params = function
  | hd :: tl ->
      (match hd with
      | Node.Char _
      | Node.Dir
      | Node.ExprParam _
      | Node.Star
      | Node.StarStar -> expand_branch (params @ [hd]) tl
      | Node.Branch branches ->
          let heads = List.concat (List.map (expand_branch params) branches) in
          List.concat (List.map (fun pat -> expand_branch pat tl) heads))
  | [] -> [params]

let rec compile_params oplist make_op = function
  | hd :: tl ->
      let expr_params = find_expr_param [] hd in
      number_expr_params 0 expr_params;
      compile_exprs oplist (List.map (fun p -> p.Node.ep_expr) expr_params);
      let params = List.map Param.create (expand_branch [] hd) in
      OpList.add oplist (make_op (List.length expr_params) params);
      compile_params oplist make_op tl
  | [] -> ()
and compile_command_params oplist =
  compile_params oplist (fun n l -> Op.PushCommandParams (n, l))
and compile_every_params oplist =
  compile_params oplist (fun n l -> Op.PushParams (n, l))
and add_command oplist params path flags =
  OpList.add oplist (Op.PushConst path);
  OpList.add oplist (Op.PushCommand flags);
  compile_command_params oplist params
and compile_commands oplist = function
    (params, _, _, Some Node.Dup) :: tl ->
      OpList.add oplist Op.PushCommandE2O;
      compile_command_params oplist params;
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
  | Node.String contents ->
      List.iter (compile_expr oplist) contents;
      OpList.add oplist (Op.Concat (List.length contents))
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
  compile_every_params oplist (List.rev params);
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
      compile_command_params oplist left;
      OpList.add oplist (Op.PushConst Value.Nil);
      OpList.add oplist (Op.PushCommand []);
      compile_command_params oplist right;
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
