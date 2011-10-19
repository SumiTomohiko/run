
module Op = struct
  type t = Char of char | Dir | Expr of int | NotDot | Star | StarStar
end

module Compiler = struct
  let compile_node = function
    | Node.Char c -> Op.Char c
    | Node.Dir -> Op.Dir
    | Node.ExprParam { Node.ep_index } -> Op.Expr ep_index
    | Node.Star -> Op.Star
    | Node.StarStar -> Op.StarStar
    | _ -> failwith "Invalid node"

  let compile_first_node = function
    | Node.Star -> [Op.NotDot]
    | _ -> []

  let compile nodes =
    (compile_first_node (List.hd nodes)) @ (List.map compile_node nodes)
end

type t = Op.t list

let rec try_star pattern vals name index star_end =
  if star_end < index then
    false, []
  else
    match try_pattern pattern vals name star_end with
    | true, next -> true, next
    | false, _ -> try_star pattern vals name index (star_end - 1)
and try_pattern pattern vals name index =
  let size = String.length name in
  if index = size then
    match pattern with
    | Op.Dir :: tl -> true, tl
    | [] -> true, []
    | _ -> false, []
  else
    match pattern with
    | (Op.Char c) :: tl ->
        if (String.get name index) = c then
          try_pattern tl vals name (index + 1)
        else
          false, []
    | (Op.Expr n) :: tl ->
        let v = Array.get vals n in
        let len = String.length v in
        if ((size - index) < len) || ((String.sub name index len) <> v) then
          false, []
        else
          try_pattern tl vals name (index + (String.length v))
    | Op.NotDot :: tl ->
        if (String.get name index) = '.' then
          false, []
        else
          try_pattern tl vals name index
    | Op.Star :: tl -> try_star tl vals name index size
    | Op.StarStar :: _ -> true, pattern
    | [] -> false, []
    | _ -> failwith "Invalid operation"

let rec traverse dir pattern vals =
  try
    let dirp = Unix.opendir dir in
    let cleanup () = Unix.closedir dirp in
    let rec loop founds =
      try
        match Unix.readdir dirp with
        | "."
        | ".." -> loop founds
        | name ->
            let path = Printf.sprintf "%s/%s" dir name in
            let matched = find_abs path vals pattern in
            let matched2 = if (Unix.stat path).Unix.st_kind = Unix.S_DIR then
              traverse path pattern vals
            else
              [] in
            loop (founds @ matched @ matched2)
      with
      | End_of_file -> founds in
    Std.finally cleanup loop []
  with
  | Unix.Unix_error (Unix.ENOTDIR, _, _) -> []
and find_at dirp dir pattern vals founds =
  try
    let matched = match Unix.readdir dirp with
    | "."
    | ".." -> []
    | name ->
        let path = Printf.sprintf "%s/%s" dir name in
        match try_pattern pattern vals name 0 with
        | true, [] -> [path]
        | true, child -> find_abs path vals child
        | false, _ -> [] in
    find_at dirp dir pattern vals (founds @ matched)
  with
  | End_of_file -> founds
and find_abs dir vals = function
  | Op.StarStar :: Op.Dir :: tl -> traverse dir tl vals
  | pattern ->
      try
        let dirp = Unix.opendir dir in
        let cleanup () = Unix.closedir dirp in
        Std.finally cleanup (fun () -> find_at dirp dir pattern vals []) ()
      with
      | Unix.Unix_error (Unix.ENOTDIR, _, _) -> []

let find dir pattern vals =
  let size = (String.length dir) + 1 in
  let remove_dir path = String.sub path size ((String.length path) - size) in
  let pathes = find_abs dir (Array.map Value.string_of_value vals) pattern in
  List.map remove_dir pathes

let rec expand_branch pattern = function
  | hd :: tl ->
      (match hd with
      | Node.Char _
      | Node.Dir
      | Node.ExprParam _
      | Node.Star
      | Node.StarStar -> expand_branch (pattern @ [hd]) tl
      | Node.Branch branches ->
          let heads = List.concat (List.map (expand_branch pattern) branches) in
          List.concat (List.map (fun pat -> expand_branch pat tl) heads))
  | [] -> [pattern]

let rec is_static = function
  | (Node.Char _) :: tl -> is_static tl
  | Node.Dir :: tl -> is_static tl
  | _ :: _ -> false
  | [] -> true

let rec join_chars s = function
  | (Node.Char c) :: tl -> join_chars (s ^ (String.make 1 c)) tl
  | Node.Dir :: tl -> join_chars (s ^ "/") tl
  | [] -> s
  | _ -> failwith "Invalid Node"

let compile = Compiler.compile

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
