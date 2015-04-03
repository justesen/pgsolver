{
open Mucalcparser

let get_label s = String.sub s 1 (String.length s - 2)

let keyword = function
    | "mu"  -> MU
    | "nu"  -> NU
    | "and" -> AND
    | "or"  -> OR
    | "TT"  -> TRUE
    | "FF"  -> FALSE
    | s     -> VAR s
}

let digit    = ['0'-'9']
let char     = ['A'-'Z' 'a'-'z']
let ident    = char (char | digit | ['_'])*
let diamond  = '<' ident '>'
let box      = '[' ident ']'

rule token = parse
      [' ' '\t' '\n']   { token lexbuf }     (* skip blanks *)
    | '.'               { DOT }
    | diamond           { EX (get_label (Lexing.lexeme lexbuf)) }
    | box               { FA (get_label (Lexing.lexeme lexbuf)) }
    | '~'               { NEG }
    | '('               { LPAR }
    | ')'               { RPAR }
    | ident             { keyword (Lexing.lexeme lexbuf) }
    | eof               { EOF }
