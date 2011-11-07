{
let trim = ExtString.String.strip
}

rule token = parse
    eof { InlineNode.Eof }
  | "``" ([^'`']* as s) "``" { InlineNode.Literal s }
  | ":doc:`" ([^'`']+ as name) '`' { InlineNode.Reference name }
  | "::" as s { InlineNode.Plain s }
  | ':' as c { InlineNode.Plain (String.make 1 c) }
  | '`' ([^'<']* as text) '<' ([^'`']* as url) ">`_" {
    InlineNode.Link (trim text, trim url)
  }
  | [^'`' ':']* as s { InlineNode.Plain s }

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
