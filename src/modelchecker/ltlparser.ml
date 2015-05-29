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

open Parsing;;
let _ = parse_error;;
# 2 "src/modelchecker/ltlparser.mly"
open Ltl
# 23 "src/modelchecker/ltlparser.ml"
let yytransl_const = [|
  257 (* UNTIL *);
  258 (* NEXT *);
  259 (* EVENTUALLY *);
  260 (* ALWAYS *);
  261 (* AND *);
  262 (* OR *);
  263 (* NEG *);
  264 (* IMPL *);
  266 (* TRUE *);
  267 (* FALSE *);
  268 (* LPAR *);
  269 (* RPAR *);
  270 (* EOL *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  265 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\003\000\003\000\002\000\002\000\003\000\
\002\000\002\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\011\000\012\000\
\013\000\000\000\014\000\000\000\007\000\009\000\010\000\006\000\
\000\000\000\000\000\000\000\000\000\000\001\000\002\000\008\000\
\000\000\000\000\000\000"

let yydgoto = "\002\000\
\011\000\012\000"

let yysindex = "\255\255\
\031\255\000\000\031\255\031\255\031\255\031\255\000\000\000\000\
\000\000\031\255\000\000\011\000\000\000\000\000\000\000\000\000\
\024\255\031\255\031\255\031\255\031\255\000\000\000\000\000\000\
\002\255\002\255\009\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\005\000\002\000"

let yygindex = "\000\000\
\000\000\003\000"

let yytablesize = 275
let yytable = "\001\000\
\004\000\003\000\018\000\000\000\005\000\013\000\014\000\015\000\
\016\000\018\000\022\000\000\000\017\000\019\000\020\000\000\000\
\021\000\000\000\000\000\000\000\024\000\025\000\026\000\027\000\
\018\000\000\000\000\000\000\000\019\000\020\000\000\000\021\000\
\003\000\004\000\005\000\000\000\023\000\006\000\000\000\007\000\
\008\000\009\000\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\004\000\000\000\
\004\000\005\000\005\000\018\000\005\000\004\000\003\000\019\000\
\020\000\005\000\021\000"

let yycheck = "\001\000\
\000\000\000\000\001\001\255\255\000\000\003\000\004\000\005\000\
\006\000\001\001\000\000\255\255\010\000\005\001\006\001\255\255\
\008\001\255\255\255\255\255\255\018\000\019\000\020\000\021\000\
\001\001\255\255\255\255\255\255\005\001\006\001\255\255\008\001\
\002\001\003\001\004\001\255\255\013\001\007\001\255\255\009\001\
\010\001\011\001\012\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\005\001\006\001\255\255\
\008\001\005\001\006\001\001\001\008\001\013\001\013\001\005\001\
\006\001\013\001\008\001"

let yynames_const = "\
  UNTIL\000\
  NEXT\000\
  EVENTUALLY\000\
  ALWAYS\000\
  AND\000\
  OR\000\
  NEG\000\
  IMPL\000\
  TRUE\000\
  FALSE\000\
  LPAR\000\
  RPAR\000\
  EOL\000\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 26 "src/modelchecker/ltlparser.mly"
                            ( _1 )
# 180 "src/modelchecker/ltlparser.ml"
               : Ltl.ltlexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 29 "src/modelchecker/ltlparser.mly"
                            ( _2 )
# 187 "src/modelchecker/ltlparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 30 "src/modelchecker/ltlparser.mly"
                            ( Dis (Neg _1, _3) )
# 195 "src/modelchecker/ltlparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 31 "src/modelchecker/ltlparser.mly"
                            ( Con (_1, _3) )
# 203 "src/modelchecker/ltlparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 32 "src/modelchecker/ltlparser.mly"
                            ( Dis (_1, _3) )
# 211 "src/modelchecker/ltlparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "src/modelchecker/ltlparser.mly"
                            ( Neg _2 )
# 218 "src/modelchecker/ltlparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "src/modelchecker/ltlparser.mly"
                            ( Next _2 )
# 225 "src/modelchecker/ltlparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 35 "src/modelchecker/ltlparser.mly"
                            ( Until (_1, _3) )
# 233 "src/modelchecker/ltlparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "src/modelchecker/ltlparser.mly"
                            ( Eventually _2 )
# 240 "src/modelchecker/ltlparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "src/modelchecker/ltlparser.mly"
                            ( Always _2 )
# 247 "src/modelchecker/ltlparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "src/modelchecker/ltlparser.mly"
                            ( Var _1 )
# 254 "src/modelchecker/ltlparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "src/modelchecker/ltlparser.mly"
                            ( TT )
# 260 "src/modelchecker/ltlparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "src/modelchecker/ltlparser.mly"
                            ( FF )
# 266 "src/modelchecker/ltlparser.ml"
               : 'expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ltl.ltlexpr)
