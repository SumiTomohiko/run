type token =
  | EOF
  | EQUAL
  | FALSE
  | TRUE

val script :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Node.t list
