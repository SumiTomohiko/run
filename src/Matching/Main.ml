
type pattern = Op.t list
type t = Static of string | Dynamic of pattern

let rec try_star pattern name index star_end =
  if star_end < index then
    false
  else
    if try_pattern pattern name star_end then
      true
    else
      try_star pattern name index (star_end - 1)
and try_pattern pattern name index =
  let size = String.length name in
  if index = size then
    (List.length pattern) = 0
  else
    match pattern with
      (Op.Char c) :: tl ->
        if (String.get name index) = c then
          try_pattern tl name (index + 1)
        else
          false
    | Op.NotDot :: tl ->
        if (String.get name index) = '.' then
          false
        else
          try_pattern tl name index
    | Op.Star :: tl -> try_star tl name index size
    | [] -> false
    | _ -> failwith "Invalid operation"

let rec find_at dirp pattern founds =
  try
    let matched = match Unix.readdir dirp with
      "."
    | ".." -> []
    | name -> if try_pattern pattern name 0 then [name] else [] in
    find_at dirp pattern (founds @ matched)
  with
    End_of_file -> founds

let find pattern =
  let dirp = Unix.opendir (Unix.getcwd ()) in
  let cleanup () = Unix.closedir dirp in
  Std.finally cleanup (fun () -> find_at dirp pattern []) ()

let rec expand_branch pattern = function
    hd :: tl ->
      (match hd with
        Node.Char _
      | Node.Star
      | Node.StarStar -> expand_branch (pattern @ [hd]) tl
      | Node.Branch branches ->
          let heads = List.concat (List.map (expand_branch pattern) branches) in
          List.concat (List.map (fun pat -> expand_branch pat tl) heads))
  | [] -> [pattern]

let rec is_static = function
    (Node.Char _) :: tl -> is_static tl
  | _ :: _ -> false
  | [] -> true

let rec join_chars = function
    (Node.Char c) :: tl -> (String.make 1 c) ^ (join_chars tl)
  | [] -> ""
  | _ -> failwith "Invalid Node"

let compile_pattern pattern =
  if is_static pattern then
    Static (join_chars pattern)
  else
    Dynamic (Compiler.compile pattern)

let compile lexbuf =
  let branches = expand_branch [] (Parser.pattern Lexer.token lexbuf) in
  List.map compile_pattern branches

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
