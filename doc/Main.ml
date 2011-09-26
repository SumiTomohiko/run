
type page = { title: string; nodes: BlockNode.t list }

type generator = {
  mutable section_depth: int;
  mutable list_depth: int Stack.t;
  waiting_queue: string DynArray.t;
  mutable pages: (string, page) Hashtbl.t
}

let escape_char = function
    '&' -> "&amp;"
  | '<' -> "&lt;"
  | '>' -> "&gt;"
  | '"' -> "&quot;"
  | c -> String.make 1 c

let escape_html s =
  let chars = DynArray.create () in
  String.iter (DynArray.add chars) s;
  String.concat "" (DynArray.to_list (DynArray.map escape_char chars))

let convert_inline generator = function
    InlineNode.Plain s -> escape_html s
  | InlineNode.Literal s -> "<code>" ^ (escape_html s) ^ "</code>"
  | InlineNode.Eof -> ""
  | InlineNode.Reference name ->
      let title = (Hashtbl.find generator.pages name).title in
      Printf.sprintf "<a href=\"%s.html\">%s</a>" (escape_html name) title

let convert_inlines generator nodes =
  String.concat "" (List.map (convert_inline generator) nodes)

let rec close_sections generator depth =
  let current_depth = generator.section_depth in
  if current_depth < depth then
    ""
  else begin
    generator.section_depth <- current_depth - 1;
    if current_depth = 1 then
      ""
    else
      "</section>\n" ^ (close_sections generator depth)
  end

let open_section generator =
  generator.section_depth <- generator.section_depth + 1;
  if generator.section_depth = 1 then "" else "<section>\n"

let enclose_tag tag text = Printf.sprintf "<%s>%s</%s>" tag text tag

let convert_title generator depth nodes =
  let closing = close_sections generator depth in
  let opening = open_section generator in
  closing ^ opening ^ (enclose_tag "h1" (convert_inlines generator nodes))

let open_list generator depth =
  let current_depth = Stack.top generator.list_depth in
  if current_depth < depth then begin
    Stack.push depth generator.list_depth;
    "<ul>\n"
  end else
    ""

let rec close_list generator tags =
  let stack = generator.list_depth in
  if (Stack.length stack) = 1 then
    tags
  else begin
    ignore (Stack.pop stack);
    close_list generator (tags ^ "</ul>\n")
  end

let convert_nonlist_block generator = function
    BlockNode.BulletItem _ -> assert false
  | BlockNode.Paragraph nodes ->
      enclose_tag "p" (convert_inlines generator nodes)
  | BlockNode.Title (depth, nodes) ->
      convert_title generator depth nodes

let convert_block generator = function
    BlockNode.BulletItem (depth, nodes) ->
      let opening = open_list generator depth in
      opening ^ (enclose_tag "li" (convert_inlines generator nodes))
  | node ->
      let list_closing = close_list generator "" in
      list_closing ^ (convert_nonlist_block generator node)

let generate generator ch node =
  output_string ch ((convert_block generator node) ^ "\n")

let generate_header ch title =
  output_string ch (Printf.sprintf "<!DOCTYPE html>
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>
<link href=\"default.css\" rel=\"stylesheet\" type=\"text/css\"/>
<title>run Documentation</title>
</head>
<body>
<header><a href=\"index.html\">run Documentation - %s</a></header>
" (escape_html title))

let generate_footer ch =
  output_string ch "</body>
</html>"

let parse ch =
  let lexer = BlockLexer.make_lexer () in
  let lexbuf = Lexing.from_channel ch in
  Parser.rst (BlockLexer.token lexer) lexbuf

let change_extension path = (Filename.chop_extension path) ^ ".html"

let generate_page generator ch title nodes =
  generate_header ch title;
  List.iter (generate generator ch) nodes;
  output_string ch (close_sections generator 1);
  generate_footer ch

let rec find_all_references_inline founds = function
    hd :: tl ->
      let names = match hd with
        InlineNode.Eof
      | InlineNode.Literal _
      | InlineNode.Plain _ -> []
      | InlineNode.Reference name -> [name] in
      find_all_references_inline (founds @ names) tl
  | [] -> founds

let rec find_all_references founds = function
    hd :: tl ->
      let nodes = match hd with
        BlockNode.BulletItem (_, nodes) -> nodes
      | BlockNode.Paragraph nodes -> nodes
      | BlockNode.Title (_, nodes) -> nodes in
      let names = find_all_references_inline [] nodes in
      find_all_references (founds @ names) tl
  | [] -> founds

let rec read_all_pages generator hash =
  if DynArray.empty generator.waiting_queue then
    ()
  else
    let queue = generator.waiting_queue in
    let name = DynArray.get queue 0 in
    DynArray.delete queue 0;
    let inch = open_in (name ^ ".rst") in
    let nodes = Std.finally (fun () -> close_in inch) parse inch in
    Hashtbl.add hash name nodes;
    let mem name = not (Hashtbl.mem hash name) in
    let pages = List.filter mem (find_all_references [] nodes) in
    List.iter (DynArray.add queue) pages;
    read_all_pages generator hash

let rec plain_text_of_inline_node s = function
    hd :: tl ->
      let t = match hd with
        InlineNode.Plain s -> s
      | InlineNode.Literal s -> s
      | InlineNode.Eof -> ""
      | InlineNode.Reference name -> name in
      plain_text_of_inline_node (s ^ t) tl
  | [] -> s

let rec find_title = function
    hd :: tl ->
      (match hd with
        BlockNode.Title (_, nodes) -> plain_text_of_inline_node "" nodes
      | _ -> find_title tl)
  | [] -> failwith "Title not found"

let register_title name2title name nodes =
  Hashtbl.add name2title name (find_title nodes)

let read_titles name2nodes name2title =
  Hashtbl.iter (register_title name2title) name2nodes

let register_page dest name2title name nodes =
  let page = {
    title=Hashtbl.find name2title name;
    nodes=nodes } in
  Hashtbl.add dest name page

let output_page generator name page =
  let outch = open_out (name ^ ".html") in
  let title = page.title in
  let nodes = page.nodes in
  let f = generate_page generator outch title in
  Std.finally (fun () -> close_out outch) f nodes

let output_all_pages generator =
  Hashtbl.iter (output_page generator) generator.pages

let main () =
  let generator = {
    section_depth=1;
    list_depth=Stack.create ();
    waiting_queue=DynArray.create ();
    pages=Hashtbl.create 16 } in
  let name = (Filename.chop_extension (Array.get Sys.argv 1)) in
  DynArray.add generator.waiting_queue name;
  let name2nodes = Hashtbl.create 16 in
  read_all_pages generator name2nodes;
  let name2title = Hashtbl.create 16 in
  read_titles name2nodes name2title;
  let pages = Hashtbl.create 16 in
  Hashtbl.iter (register_page pages name2title) name2nodes;
  generator.pages <- pages;
  Stack.push (-1) generator.list_depth; (* -1 is a sentinel *)
  output_all_pages generator

let _ = main ()

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
