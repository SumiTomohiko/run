
rule token = parse
    eof { InlineNode.Eof }
  | "``" ([^'`']* as s) "``" { InlineNode.Literal s }
  | ":doc:`" ([^'`']+ as name) '`' { InlineNode.Reference name }
  | "::" as s { InlineNode.Plain s }
  | [^'`' ':']* as s { InlineNode.Plain s }

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
