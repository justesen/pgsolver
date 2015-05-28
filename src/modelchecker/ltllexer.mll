{
open Ltlparser

let keyword = function
    | "and" -> AND
    | "or"  -> OR
    | "TT"  -> TRUE
    | "FF"  -> FALSE
    | s     -> VAR s
}

let digit    = ['0'-'9']
let char     = ['A'-'Z' 'a'-'z']
let ident    = char (char | digit | ['_'])*

rule token = parse
      [' ' '\t' '\n']   { token lexbuf }     (* skip blanks *)
    | 'U'               { UNTIL }
    | 'O'               { NEXT }
    | "<>"              { EVENTUALLY }
    | "[]"              { ALWAYS }
    | "->"              { IMPL }
    | '~'               { NEG }
    | '('               { LPAR }
    | ')'               { RPAR }
    | ident             { keyword (Lexing.lexeme lexbuf) }
    | eof               { EOF }
