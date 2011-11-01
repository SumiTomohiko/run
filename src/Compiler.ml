
type compiler = { oplist: OpList.t; while_stack: (Op.t * Op.t) Stack.t }

let add_op compiler = OpList.add compiler.oplist

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

let rec compile_params compiler make_op = function
  | hd :: tl ->
      let expr_params = find_expr_param [] hd in
      number_expr_params 0 expr_params;
      compile_exprs compiler (List.map (fun p -> p.Node.ep_expr) expr_params);
      let params = List.map Param.create (expand_branch [] hd) in
      add_op compiler (make_op (List.length expr_params) params);
      compile_params compiler make_op tl
  | [] -> ()
and compile_command_params compiler =
  compile_params compiler (fun n l -> Op.PushCommandParams (n, l))
and compile_every_params compiler =
  compile_params compiler (fun n l -> Op.PushParams (n, l))
and add_command compiler params path flags =
  add_op compiler (Op.PushConst path);
  add_op compiler (Op.PushCommand flags);
  compile_command_params compiler params
and compile_commands compiler = function
    (params, _, _, Some Node.Dup) :: tl ->
      add_op compiler Op.PushCommandE2O;
      compile_command_params compiler params;
      compile_commands compiler tl
  | (params, _, _, Some (Node.File (path, flags))) :: tl ->
      add_command compiler params (Value.String path) flags;
      compile_commands compiler tl
  | (params, _, _, None) :: tl ->
      add_command compiler params Value.Nil [];
      compile_commands compiler tl
  | [] -> ()
and compile_pipeline compiler commands last_op =
  let _, first_stdin, _, _ = List.hd commands in
  let stdin_path = match first_stdin with
    Some path -> Value.String path
  | None -> Value.Nil in
  add_op compiler (Op.PushConst stdin_path);
  let _, _, last_stdout, _ = List.hd (List.rev commands) in
  let stdout_path, flags = match last_stdout with
    Some (Node.File (path, flags)) -> Value.String path, flags
  | None -> Value.Nil, []
  | Some _ -> failwith "Invalid stdout redirect" in
  add_op compiler (Op.PushConst stdout_path);
  add_op compiler (Op.PushPipeline flags);
  compile_commands compiler commands;
  add_op compiler last_op
and compile_expr compiler expr =
  let _, _, body = expr in
  match body with
    Node.Add operands -> compile_binop compiler operands Op.Add
  | Node.Array exprs ->
      compile_exprs compiler exprs;
      add_op compiler (Op.MakeArray (List.length exprs))
  | Node.Assign { Node.left; Node.right } ->
      compile_expr compiler right;
      (match left with
      | _, _, Node.Subscript { Node.prefix; Node.index } ->
          compile_expr compiler prefix;
          compile_expr compiler index;
          add_op compiler Op.StoreSubscript
      | _, _, Node.Var name ->
          add_op compiler (Op.StoreLocal name);
          add_op compiler (Op.PushLocal name)
      | _ -> failwith "Unsupported assign expression")
  | Node.Attr { Node.attr_prefix; Node.attr_name } ->
      compile_expr compiler attr_prefix;
      add_op compiler (Op.GetAttr attr_name)
  | Node.Call { Node.callee; Node.args } ->
      compile_expr compiler callee;
      compile_exprs compiler args;
      add_op compiler (Op.Call (List.length args))
  | Node.Const v -> add_op compiler (Op.PushConst v)
  | Node.Dict pairs ->
      let compile_pair { Node.key; Node.value } =
        compile_expr compiler key;
        compile_expr compiler value in
      List.iter compile_pair pairs;
      add_op compiler (Op.MakeDict (List.length pairs))
  | Node.Div operands -> compile_binop compiler operands Op.Div
  | Node.DivDiv operands -> compile_binop compiler operands Op.DivDiv
  | Node.EqualEqual operands -> compile_binop compiler operands Op.Equal
  | Node.Greater operands -> compile_binop compiler operands Op.Greater
  | Node.GreaterEqual operands ->
      compile_binop compiler operands Op.GreaterEqual
  | Node.Heredoc buf ->
      let s = Buffer.contents buf in
      add_op compiler (Op.PushConst (Value.String s))
  | Node.InlinePipeline commands ->
      compile_pipeline compiler commands Op.ExecAndPush
  | Node.LastStatus -> add_op compiler Op.PushLastStatus
  | Node.Less operands -> compile_binop compiler operands Op.Less
  | Node.LessEqual operands -> compile_binop compiler operands Op.LessEqual
  | Node.Mul operands -> compile_binop compiler operands Op.Mul
  | Node.NotEqual operands -> compile_binop compiler operands Op.NotEqual
  | Node.String contents ->
      List.iter (compile_expr compiler) contents;
      add_op compiler (Op.Concat (List.length contents))
  | Node.Sub operands -> compile_binop compiler operands Op.Sub
  | Node.Subscript { Node.prefix; Node.index } ->
      compile_expr compiler prefix;
      compile_expr compiler index;
      add_op compiler Op.Subscript
  | Node.Var name -> add_op compiler (Op.PushLocal name)
and compile_binop compiler { Node.left; Node.right } op =
  compile_expr compiler left;
  compile_expr compiler right;
  add_op compiler op
and compile_exprs compiler = function
  | expr :: exprs ->
      compile_expr compiler expr;
      compile_exprs compiler exprs
  | [] -> ()

let rec compile_every compiler { Node.params; Node.names; Node.stmts } =
  let push_false _ = add_op compiler (Op.PushConst (Value.Bool false)) in
  List.iter push_false names;
  compile_every_params compiler (List.rev params);
  let last = Op.make_label () in
  let top = Op.make_label () in
  OpList.add_op compiler.oplist top;
  add_op compiler (Op.JumpIfFalse last);
  let store name = add_op compiler (Op.StoreLocal name) in
  List.iter store names;
  compile_stmts compiler stmts;
  add_op compiler (Op.Jump top);
  OpList.add_op compiler.oplist last
and compile_stmt compiler = function
  | Node.Break ->
      let _, label = Stack.top compiler.while_stack in
      add_op compiler (Op.Jump label)
  | Node.Communication (left, right) ->
      add_op compiler (Op.PushConst Value.Nil);
      add_op compiler (Op.PushConst Value.Nil);
      add_op compiler (Op.PushPipeline []);
      add_op compiler (Op.PushConst Value.Nil);
      add_op compiler (Op.PushCommand []);
      compile_command_params compiler left;
      add_op compiler (Op.PushConst Value.Nil);
      add_op compiler (Op.PushCommand []);
      compile_command_params compiler right;
      add_op compiler Op.Communicate
  | Node.Expr expr ->
      compile_expr compiler expr;
      add_op compiler Op.Pop
  | Node.Every every -> compile_every compiler every
  | Node.If (expr, stmts1, stmts2) ->
      let else_begin = Op.make_label () in
      let else_end = Op.make_label () in
      compile_expr compiler expr;
      add_op compiler (Op.JumpIfFalse else_begin);
      compile_stmts compiler stmts1;
      add_op compiler (Op.Jump else_end);
      OpList.add_op compiler.oplist else_begin;
      compile_stmts compiler stmts2;
      OpList.add_op compiler.oplist else_end
  | Node.Next ->
      let label, _ = Stack.top compiler.while_stack in
      add_op compiler (Op.Jump label)
  | Node.Pipeline commands -> compile_pipeline compiler commands Op.Exec
  | Node.Raise _ -> failwith "TODO: Implement here"
  | Node.Return expr ->
      compile_expr compiler expr;
      add_op compiler Op.Return
  | Node.Try (_, _, _) -> failwith "TODO: Implement here"
  | Node.UserFunction { Node.uf_name; Node.uf_args; Node.uf_stmts } ->
      let child = { oplist=OpList.make (); while_stack=Stack.create () } in
      compile_stmts child uf_stmts;
      let func_ops_index = Op.register_ops (OpList.top child.oplist) in
      add_op compiler (Op.MakeUserFunction (uf_args, func_ops_index));
      add_op compiler (Op.StoreLocal uf_name)
  | Node.While (expr, stmts) ->
      let while_begin = Op.make_label () in
      let while_end = Op.make_label () in
      Stack.push (while_begin, while_end) compiler.while_stack;
      OpList.add_op compiler.oplist while_begin;
      compile_expr compiler expr;
      add_op compiler (Op.JumpIfFalse while_end);
      compile_stmts compiler stmts;
      add_op compiler (Op.Jump while_begin);
      OpList.add_op compiler.oplist while_end;
      ignore (Stack.pop compiler.while_stack)
  | Node.Empty -> ()
and compile_stmts compiler = function
  | hd :: tl ->
      compile_stmt compiler hd;
      compile_stmts compiler tl
  | [] -> ()

let compile stmts =
  let compiler = { oplist=OpList.make (); while_stack=Stack.create () } in
  compile_stmts compiler stmts;
  OpList.top compiler.oplist

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
