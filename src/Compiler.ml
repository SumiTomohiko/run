
type compiler = {
  path: string;
  codes: Code.t DynArray.t;
  oplist: OpList.t;
  while_stack: (Op.t * Op.t) Stack.t }

let add_op compiler = OpList.add_op compiler.oplist
let add_label compiler = OpList.add_label compiler.oplist

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

let conv_kind = function
  | Op.Add -> Op.Add
  | Op.Call n -> Op.Call n
  | Op.Communicate -> Op.Communicate
  | Op.Concat n -> Op.Concat n
  | Op.Div -> Op.Div
  | Op.DivDiv -> Op.DivDiv
  | Op.Equal -> Op.Equal
  | Op.Exec -> Op.Exec
  | Op.ExecAndPush -> Op.ExecAndPush
  | Op.GetAttr s -> Op.GetAttr s
  | Op.Greater -> Op.Greater
  | Op.GreaterEqual -> Op.GreaterEqual
  | Op.Jump dest -> Op.Jump dest.Op.index
  | Op.JumpIfFalse dest -> Op.JumpIfFalse dest.Op.index
  | Op.Less -> Op.Less
  | Op.LessEqual -> Op.LessEqual
  | Op.MakeArray n -> Op.MakeArray n
  | Op.MakeDict n -> Op.MakeDict n
  | Op.MakeUserFunction (args, index) -> Op.MakeUserFunction (args, index)
  | Op.MoveParam -> Op.MoveParam
  | Op.Mul -> Op.Mul
  | Op.NotEqual -> Op.NotEqual
  | Op.Pop -> Op.Pop
  | Op.PushCommand flags -> Op.PushCommand flags
  | Op.PushCommandE2O -> Op.PushCommandE2O
  | Op.PushCommandParams (n, params) -> Op.PushCommandParams (n, params)
  | Op.PushConst v -> Op.PushConst v
  | Op.PushLastStatus -> Op.PushLastStatus
  | Op.PushLocal name -> Op.PushLocal name
  | Op.PushParams (n, params) -> Op.PushParams (n, params)
  | Op.PushPipeline flags -> Op.PushPipeline flags
  | Op.Return -> Op.Return
  | Op.StoreLocal name -> Op.StoreLocal name
  | Op.StoreSubscript -> Op.StoreSubscript
  | Op.Sub -> Op.Sub
  | Op.Subscript -> Op.Subscript
  | _ -> assert false

let rec ops_to_list accum = function
  | Some { Op.kind=Op.Anchor; Op.next }
  | Some { Op.kind=Op.Label; Op.next } -> ops_to_list accum next
  | Some { Op.kind; Op.next } -> ops_to_list (accum @ [conv_kind kind]) next
  | None -> accum

let rec make_lineno_table tbl from_index = function
  | Some { Op.kind=Op.Anchor; Op.next=Some { Op.index } as next } ->
      make_lineno_table tbl index next
  | Some { Op.pos=(_, lineno); Op.index; Op.next=None } ->
      tbl @ [(from_index, index + 1, lineno)]
  | Some {
    Op.pos=(_, lineno1);
    Op.next=Some { Op.pos=(_, lineno2) } as next } when lineno1 = lineno2 ->
      make_lineno_table tbl from_index next
  | Some {
    Op.pos=(_, lineno);
    Op.index=index1;
    Op.next=Some { Op.index=index2 } as next } ->
      make_lineno_table (tbl @ [(from_index, index1 + 1, lineno)]) index2 next
  | None -> assert false

let code_of_ops path ops =
  let tbl = Array.of_list (make_lineno_table [] 0 ops) in
  let opseq = Array.of_list (ops_to_list [] ops) in
  path, tbl, opseq

let rec compile_params compiler pos make_op = function
  | hd :: tl ->
      let expr_params = find_expr_param [] hd in
      number_expr_params 0 expr_params;
      compile_exprs compiler (List.map (fun p -> p.Node.ep_expr) expr_params);
      let params = List.map Param.create (expand_branch [] hd) in
      add_op compiler (make_op (List.length expr_params) params) pos;
      compile_params compiler pos make_op tl
  | [] -> ()
and compile_command_params compiler pos =
  compile_params compiler pos (fun n l -> Op.PushCommandParams (n, l))
and compile_every_params compiler pos =
  compile_params compiler pos (fun n l -> Op.PushParams (n, l))
and add_command compiler params path flags pos =
  add_op compiler (Op.PushConst path) pos;
  add_op compiler (Op.PushCommand flags) pos;
  compile_command_params compiler pos params
and compile_commands compiler pos = function
    (params, _, _, Some Node.Dup) :: tl ->
      add_op compiler Op.PushCommandE2O pos;
      compile_command_params compiler pos params;
      compile_commands compiler pos tl
  | (params, _, _, Some (Node.File (path, flags))) :: tl ->
      add_command compiler params (Value.String path) flags pos;
      compile_commands compiler pos tl
  | (params, _, _, None) :: tl ->
      add_command compiler params Value.Nil [] pos;
      compile_commands compiler pos tl
  | [] -> ()
and compile_pipeline compiler pos commands last_op =
  let _, first_stdin, _, _ = List.hd commands in
  let stdin_path = match first_stdin with
    Some path -> Value.String path
  | None -> Value.Nil in
  add_op compiler (Op.PushConst stdin_path) pos;
  let _, _, last_stdout, _ = List.hd (List.rev commands) in
  let stdout_path, flags = match last_stdout with
    Some (Node.File (path, flags)) -> Value.String path, flags
  | None -> Value.Nil, []
  | Some _ -> failwith "Invalid stdout redirect" in
  add_op compiler (Op.PushConst stdout_path) pos;
  add_op compiler (Op.PushPipeline flags) pos;
  compile_commands compiler pos commands;
  add_op compiler last_op pos
and compile_expr compiler (pos, expr) =
  match expr with
    Node.Add operands -> compile_binop compiler operands Op.Add pos
  | Node.Array exprs ->
      compile_exprs compiler exprs;
      add_op compiler (Op.MakeArray (List.length exprs)) pos
  | Node.Assign { Node.left; Node.right } ->
      compile_expr compiler right;
      (match left with
      | pos, Node.Subscript { Node.prefix; Node.index } ->
          compile_expr compiler prefix;
          compile_expr compiler index;
          add_op compiler Op.StoreSubscript pos
      | pos, Node.Var name ->
          add_op compiler (Op.StoreLocal name) pos;
          add_op compiler (Op.PushLocal name) pos
      (* TODO: raise CompileError *)
      | _ -> failwith "Unsupported assign expression")
  | Node.Attr { Node.attr_prefix; Node.attr_name } ->
      compile_expr compiler attr_prefix;
      add_op compiler (Op.GetAttr attr_name) pos
  | Node.Call { Node.callee; Node.args } ->
      compile_expr compiler callee;
      compile_exprs compiler args;
      add_op compiler (Op.Call (List.length args)) pos
  | Node.Const v -> add_op compiler (Op.PushConst v) pos
  | Node.Dict pairs ->
      let compile_pair { Node.key; Node.value } =
        compile_expr compiler key;
        compile_expr compiler value in
      List.iter compile_pair pairs;
      add_op compiler (Op.MakeDict (List.length pairs)) pos
  | Node.Div operands -> compile_binop compiler operands Op.Div pos
  | Node.DivDiv operands -> compile_binop compiler operands Op.DivDiv pos
  | Node.EqualEqual operands ->
      compile_binop compiler operands Op.Equal pos
  | Node.Greater operands ->
      compile_binop compiler operands Op.Greater pos
  | Node.GreaterEqual operands ->
      compile_binop compiler operands Op.GreaterEqual pos
  | Node.Heredoc buf ->
      let s = Buffer.contents buf in
      add_op compiler (Op.PushConst (Value.String s)) pos
  | Node.InlinePipeline commands ->
      compile_pipeline compiler pos commands Op.ExecAndPush
  | Node.LastStatus -> add_op compiler Op.PushLastStatus pos
  | Node.Less operands -> compile_binop compiler operands Op.Less pos
  | Node.LessEqual operands -> compile_binop compiler operands Op.LessEqual pos
  | Node.Mul operands -> compile_binop compiler operands Op.Mul pos
  | Node.NotEqual operands -> compile_binop compiler operands Op.NotEqual pos
  | Node.String contents ->
      List.iter (compile_expr compiler) contents;
      add_op compiler (Op.Concat (List.length contents)) pos
  | Node.Sub operands -> compile_binop compiler operands Op.Sub pos
  | Node.Subscript { Node.prefix; Node.index } ->
      compile_expr compiler prefix;
      compile_expr compiler index;
      add_op compiler Op.Subscript pos
  | Node.Var name -> add_op compiler (Op.PushLocal name) pos
and compile_binop compiler { Node.left; Node.right } op pos =
  compile_expr compiler left;
  compile_expr compiler right;
  add_op compiler op pos
and compile_exprs compiler = function
  | expr :: exprs ->
      compile_expr compiler expr;
      compile_exprs compiler exprs
  | [] -> ()

let rec compile_every compiler pos { Node.params; Node.names; Node.stmts } =
  let push_false _ = add_op compiler (Op.PushConst (Value.Bool false)) pos in
  List.iter push_false names;
  compile_every_params compiler pos (List.rev params);
  let last = Op.make_label pos in
  let top = Op.make_label pos in
  add_label compiler top;
  add_op compiler (Op.JumpIfFalse last) pos;
  let store name = add_op compiler (Op.StoreLocal name) pos in
  List.iter store names;
  compile_stmts compiler stmts;
  add_op compiler (Op.Jump top) pos;
  add_label compiler last
and compile_stmt compiler (pos, stmt) =
  match stmt with
  | Node.Break ->
      let _, label = Stack.top compiler.while_stack in
      add_op compiler (Op.Jump label) pos
  | Node.Communication (left, right) ->
      add_op compiler (Op.PushConst Value.Nil) pos;
      add_op compiler (Op.PushConst Value.Nil) pos;
      add_op compiler (Op.PushPipeline []) pos;
      add_op compiler (Op.PushConst Value.Nil) pos;
      add_op compiler (Op.PushCommand []) pos;
      compile_command_params compiler pos left;
      add_op compiler (Op.PushConst Value.Nil) pos;
      add_op compiler (Op.PushCommand []) pos;
      compile_command_params compiler pos right;
      add_op compiler Op.Communicate pos
  | Node.Expr ((pos, _) as expr) ->
      compile_expr compiler expr;
      add_op compiler Op.Pop pos
  | Node.Every every -> compile_every compiler pos every
  | Node.If (expr, stmts1, stmts2) ->
      let else_begin = Op.make_label pos in
      let else_end = Op.make_label pos in
      compile_expr compiler expr;
      add_op compiler (Op.JumpIfFalse else_begin) pos;
      compile_stmts compiler stmts1;
      add_op compiler (Op.Jump else_end) pos;
      add_label compiler else_begin;
      compile_stmts compiler stmts2;
      add_label compiler else_end
  | Node.Next ->
      let label, _ = Stack.top compiler.while_stack in
      add_op compiler (Op.Jump label) pos
  | Node.Pipeline commands -> compile_pipeline compiler pos commands Op.Exec
  | Node.Raise _ -> failwith "TODO: Implement here"
  | Node.Return expr ->
      compile_expr compiler expr;
      add_op compiler Op.Return pos
  | Node.Try (_, _, _) -> failwith "TODO: Implement here"
  | Node.UserFunction { Node.uf_name; Node.uf_args; Node.uf_stmts } ->
      let index = compile compiler.path compiler.codes uf_stmts in
      add_op compiler (Op.MakeUserFunction (uf_args, index)) pos;
      add_op compiler (Op.StoreLocal uf_name) pos
  | Node.While (expr, stmts) ->
      let while_begin = Op.make_label pos in
      let while_end = Op.make_label pos in
      Stack.push (while_begin, while_end) compiler.while_stack;
      add_label compiler while_begin;
      compile_expr compiler expr;
      add_op compiler (Op.JumpIfFalse while_end) pos;
      compile_stmts compiler stmts;
      add_op compiler (Op.Jump while_begin) pos;
      add_label compiler while_end;
      ignore (Stack.pop compiler.while_stack)
  | Node.Empty -> ()
and compile_stmts compiler = function
  | hd :: tl ->
      compile_stmt compiler hd;
      compile_stmts compiler tl
  | [] -> ()
and compile path codes stmts =
  let compiler = {
    path=path;
    codes=codes;
    oplist=OpList.make ();
    while_stack=Stack.create () } in
  compile_stmts compiler stmts;
  DynArray.add codes (code_of_ops path (Some (OpList.top compiler.oplist)));
  (DynArray.length codes) - 1

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
