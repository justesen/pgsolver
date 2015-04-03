type token =
  | MU
  | NU
  | DOT
  | EX of (string)
  | FA of (string)
  | AND
  | OR
  | NEG
  | VAR of (string)
  | TRUE
  | FALSE
  | LPAR
  | RPAR
  | EOL
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Mucalc.muexpr
