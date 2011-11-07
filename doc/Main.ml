
type page = { title: string; nodes: BlockNode.t list }

type generator = {
  mutable section_depth: int;
  mutable list_depth: int Stack.t;
  pages: (string, page) Hashtbl.t;
  mutable preformatted: bool }

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

let enclose_tag tag text = Printf.sprintf "<%s>%s</%s>" tag text tag

let rec get_whitespace_length s index =
  if (String.get s index) <> ' ' then
    index
  else
    get_whitespace_length s (index + 1)

let sprintf = Printf.sprintf

let make_tag name content = sprintf "<%s %s>" name content

let string_of_inline_node = function
  | InlineNode.Eof -> "<Eof>"
  | InlineNode.Literal s -> make_tag "Literal" s
  | InlineNode.Plain s -> make_tag "Plain" s
  | InlineNode.Reference s -> make_tag "Reference" s

let get_indent_depth = function
  | InlineNode.Plain s -> get_whitespace_length s 0
  | _ -> assert false

let convert_inline generator = function
    InlineNode.Plain s -> if s = "::" then ":" else escape_html s
  | InlineNode.Literal s -> enclose_tag "code" (escape_html s)
  | InlineNode.Eof -> ""
  | InlineNode.Reference name ->
      let plain_title = (Hashtbl.find generator.pages name).title in
      let title = escape_html plain_title in
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

let convert_title generator depth nodes =
  let closing = close_sections generator depth in
  let opening = open_section generator in
  closing ^ opening ^ (enclose_tag "h1" (convert_inlines generator nodes))

let list_sentinel = (-1)

let rec close_list generator depth accum =
  let stack = generator.list_depth in
  let current_depth = Stack.top stack in
  if current_depth = depth then
    accum
  else begin
    ignore (Stack.pop stack);
    close_list generator depth (accum ^ "</ul>\n")
  end

let open_or_close_list generator depth =
  let current_depth = Stack.top generator.list_depth in
  if current_depth < depth then begin
    Stack.push depth generator.list_depth;
    "<ul>\n"
  end else
    close_list generator depth ""

let rec close_all_list generator = close_list generator list_sentinel ""

let set_preformatted generator b = generator.preformatted <- b
let enable_preformatted generator = set_preformatted generator true
let disable_preformatted generator = set_preformatted generator false

let rec trim_indent accum depth = function
  | (InlineNode.Plain s) :: tl ->
      if 0 < (String.length s) then
        if (String.get s 0) = '\n' then
          let pos = depth + 1 in
          let trimmed = String.sub s pos ((String.length s) - pos) in
          trim_indent (accum @ [InlineNode.Plain ("\n" ^ trimmed)]) depth tl
        else
          let pos = depth in
          let trimmed = String.sub s pos ((String.length s) - pos) in
          trim_indent (accum @ [InlineNode.Plain trimmed]) depth tl
      else
        trim_indent accum depth tl
  | [] -> accum
  | _ -> assert false

let convert_nonlist_block generator = function
    BlockNode.BulletItem _ -> assert false
  | BlockNode.Paragraph [InlineNode.Plain "::"] ->
      enable_preformatted generator;
      ""
  | BlockNode.Paragraph nodes ->
      if generator.preformatted then begin
        disable_preformatted generator;
        let depth = get_indent_depth (List.hd nodes) in
        let l = trim_indent [] depth nodes in
        enclose_tag "pre" (convert_inlines generator l)
      end else begin
        (match List.hd (List.rev nodes) with
        | InlineNode.Plain "::" -> enable_preformatted generator
        | _ -> ());
        enclose_tag "p" (convert_inlines generator nodes)
      end
  | BlockNode.Title (depth, nodes) ->
      convert_title generator depth nodes

let convert_block generator = function
    BlockNode.BulletItem (depth, nodes) ->
      let opening = open_or_close_list generator depth in
      opening ^ (enclose_tag "li" (convert_inlines generator nodes))
  | node ->
      let list_closing = close_all_list generator in
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

let rec read_all_pages queue hash =
  if DynArray.empty queue then
    ()
  else
    let name = DynArray.get queue 0 in
    DynArray.delete queue 0;
    let inch = open_in (name ^ ".rst") in
    let nodes = Std.finally (fun () -> close_in inch) parse inch in
    Hashtbl.add hash name nodes;
    let mem name = not (Hashtbl.mem hash name) in
    let pages = List.filter mem (find_all_references [] nodes) in
    List.iter (DynArray.add queue) pages;
    read_all_pages queue hash

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
  let first_page_name = (Filename.chop_extension (Array.get Sys.argv 1)) in
  let queue = DynArray.create () in
  DynArray.add queue first_page_name;
  let hash_size = 16 in
  let name2nodes = Hashtbl.create hash_size in
  read_all_pages queue name2nodes;

  let name2title = Hashtbl.create hash_size in
  read_titles name2nodes name2title;

  let pages = Hashtbl.create hash_size in
  Hashtbl.iter (register_page pages name2title) name2nodes;

  let generator = {
    section_depth=1;
    list_depth=Stack.create ();
    pages=pages;
    preformatted=false } in
  Stack.push list_sentinel generator.list_depth;
  output_all_pages generator

let _ = main ()

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
