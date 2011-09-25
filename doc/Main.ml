
type generator = {
  mutable section_depth: int
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

let convert_inline = function
    InlineNode.Plain s -> escape_html s
  | InlineNode.Literal s -> "<code>" ^ (escape_html s) ^ "</code>"
  | InlineNode.Eof -> ""

let convert_inlines nodes =
  String.concat "" (List.map convert_inline nodes)

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
  closing ^ opening ^ "<h1>" ^ convert_inlines nodes ^ "</h1>"

let convert_block generator = function
    BlockNode.Paragraph nodes -> "<p>" ^ (convert_inlines nodes) ^ "</p>"
  | BlockNode.Title (depth, nodes) -> convert_title generator depth nodes

let generate generator ch node =
  output_string ch ((convert_block generator node) ^ "\n")

let generate_header ch =
  output_string ch "<!DOCTYPE html>
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>
<link href=\"default.css\" rel=\"stylesheet\" type=\"text/css\"/>
<title>run Documentation</title>
</head>
<body>
<header><a href=\"index.html\">run Documentation</a></header>
"

let generate_footer ch =
  output_string ch "</body>
</html>"

let parse ch =
  let lexer = BlockLexer.make_lexer () in
  let lexbuf = Lexing.from_channel ch in
  Parser.rst (BlockLexer.token lexer) lexbuf

let change_extension path = (Filename.chop_extension path) ^ ".html"

let generate_all ch nodes =
  let generator = { section_depth=1 } in
  generate_header ch;
  List.iter (generate generator ch) nodes;
  output_string ch (close_sections generator 1);
  generate_footer ch

let main () =
  let inpath = Array.get Sys.argv 1 in
  let inch = open_in inpath in
  let nodes = Std.finally (fun () -> close_in inch) parse inch in
  let outch = open_out (change_extension inpath) in
  Std.finally (fun () -> close_out outch) (generate_all outch) nodes

let _ = main ()

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)