
module Command = struct
  let find = Hashtbl.find

  let create () =
    let hash = Hashtbl.create 31 in
    List.iter (fun (name, f) -> Hashtbl.add hash name f) [];
    hash
end

module Function = struct
  let output f args =
    List.iter (fun v -> print_string (f v)) args;
    Core.Nil

  let builtins = [
    ("print", output Core.string_of_value);
    ("puts", output (fun v -> (Core.string_of_value v) ^ "\n"))]

  let create () =
    let tbl = Core.Symboltbl.create () in
    let add (name, f) = Core.Symboltbl.add tbl name (Core.Function f) in
    List.iter add builtins;
    tbl
end

module Class = struct
  module Array = struct
    let array_expand _ self _ =
      match self with
      | Core.Array a ->
          let strings = Array.to_list (Array.map Core.string_of_value a) in
          Core.value_of_string (String.concat " " strings)
      (* TODO: raise TypeError *)
      | _ -> failwith "self must be Array"

    let get_attr _ self = function
      | "size" ->
          (match self with
          | Core.Array a -> Core.Int (Num.num_of_int (Array.length a))
          (* TODO: raise TypeError *)
          | _ -> assert false)
      | "expand" -> Core.Method (self, array_expand)
      (* TODO: raise AttributeError *)
      | name -> failwith ("AttributeError: " ^ name)
  end

  module Dict = struct
    let dict_expand _ self _ =
      match self with
      | Core.Dict h ->
          let f key value init =
            let s = Core.string_of_value key in
            let t = Core.string_of_value value in
            (s ^ " " ^ t) :: init in
          Core.String (String.concat " " (Hashtbl.fold f h []))
      (* TODO: raise TypeError *)
      | _ -> failwith "self must be Dict"

    let get_attr _ self = function
      | "expand" -> Core.Method (self, dict_expand)
      (* TODO: raise AttributeError *)
      | name -> failwith ("AttributeError: " ^ name)
  end

  module Class = struct
    let get_attr _ self = function
      | "new" ->
          (match self with
          | Core.Class (_, f) -> Core.Method (self, f)
          (* TODO: raise TypeError *)
          | _ -> assert false)
      (* TODO: raise AttributeError *)
      | _ -> failwith "TODO: Raise AttributeError"
  end

  module Exception = struct
    let get_attr _ self = function
      | "message" ->
          (match self with
          | Core.Exception (_, _, msg) -> msg
          (* TODO: raise TypeError *)
          | _ -> assert false)
      | name ->
          (* TODO: raise AttributeError *)
          failwith (Printf.sprintf "TODO: Raise AttributeError of %s" name)
  end
end

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
