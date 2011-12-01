
type part = Static of string | Dynamic of int
type t = Single of part list | Matching of Matching.t

let rec is_matching_param = function
  | Node.Star :: _
  | Node.StarStar :: _ -> true
  | _ :: tl -> is_matching_param tl
  | [] -> false

let rec chain_static_chars s = function
  | (Node.Char c) :: tl -> chain_static_chars (s ^ (String.make 1 c)) tl
  | Node.Dir :: tl -> chain_static_chars (s ^ "/") tl
  | l -> s, l

let rec create_single_param parts = function
  | (Node.Branch _) :: _
  | Node.Star :: _
  | Node.StarStar :: _ -> assert false
  | Node.ExprParam { Node.ep_index } :: tl ->
      create_single_param (parts @ [Dynamic ep_index]) tl
  | [] -> Single parts
  | l ->
      let s, rest = chain_static_chars "" l in
      create_single_param (parts @ [Static s]) rest

let rec eval_single s vals = function
  | (Dynamic index) :: tl ->
      let v = Core.string_of_value (Array.get vals index) in
      eval_single (s ^ v) vals tl
  | (Static t) :: tl -> eval_single (s ^ t) vals tl
  | [] -> s

let eval vals = function
  | Matching ops -> Matching.find (Unix.getcwd ()) ops vals
  | Single parts -> [eval_single "" vals parts]

let create nodes =
  if is_matching_param nodes then
    Matching (Matching.compile nodes)
  else
    create_single_param [] nodes

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
