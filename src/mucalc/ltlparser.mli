type token =
  | UNTIL
  | NEXT
  | EVENTUALLY
  | ALWAYS
  | AND
  | OR
  | NEG
  | IMPL
  | VAR of (string)
  | TRUE
  | FALSE
  | LPAR
  | RPAR
  | EOL
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ltl.ltlexpr
