type token =
  | MU
  | NU
  | DOT
  | EX of (string)
  | FA of (string)
  | AEX
  | AFA
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

open Parsing;;
let _ = parse_error;;
# 2 "src/modelchecker/mucalcparser.mly"
open Mucalc
# 25 "src/modelchecker/mucalcparser.ml"
let yytransl_const = [|
  257 (* MU *);
  258 (* NU *);
  259 (* DOT *);
  262 (* AEX *);
  263 (* AFA *);
  264 (* AND *);
  265 (* OR *);
  266 (* NEG *);
  268 (* TRUE *);
  269 (* FALSE *);
  270 (* LPAR *);
  271 (* RPAR *);
  272 (* EOL *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  260 (* EX *);
  261 (* FA *);
  267 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\004\000\004\000\003\000\003\000\003\000\002\000\002\000\
\002\000\002\000\002\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\013\000\014\000\000\000\015\000\000\000\000\000\
\000\000\008\000\009\000\010\000\011\000\007\000\000\000\000\000\
\000\000\001\000\000\000\000\000\004\000\005\000\006\000\000\000\
\000\000"

let yydgoto = "\002\000\
\014\000\015\000"

let yysindex = "\010\000\
\027\255\000\000\245\254\004\255\027\255\027\255\027\255\027\255\
\027\255\000\000\000\000\000\000\027\255\000\000\003\000\014\255\
\015\255\000\000\000\000\000\000\000\000\000\000\001\255\027\255\
\027\255\000\000\027\255\027\255\000\000\000\000\000\000\005\255\
\005\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\002\000"

let yygindex = "\000\000\
\000\000\255\255"

let yytablesize = 273
let yytable = "\016\000\
\002\000\003\000\026\000\018\000\019\000\020\000\021\000\022\000\
\024\000\025\000\001\000\023\000\024\000\025\000\017\000\029\000\
\027\000\028\000\000\000\000\000\000\000\000\000\030\000\031\000\
\000\000\032\000\033\000\003\000\004\000\000\000\005\000\006\000\
\007\000\008\000\000\000\000\000\009\000\010\000\011\000\012\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\000\025\000\000\000\000\000\000\000\002\000\
\003\000"

let yycheck = "\011\001\
\000\000\000\000\000\000\005\000\006\000\007\000\008\000\009\000\
\008\001\009\001\001\000\013\000\008\001\009\001\011\001\015\001\
\003\001\003\001\255\255\255\255\255\255\255\255\024\000\025\000\
\255\255\027\000\028\000\001\001\002\001\255\255\004\001\005\001\
\006\001\007\001\255\255\255\255\010\001\011\001\012\001\013\001\
\014\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\008\001\009\001\255\255\255\255\255\255\015\001\
\015\001"

let yynames_const = "\
  MU\000\
  NU\000\
  DOT\000\
  AEX\000\
  AFA\000\
  AND\000\
  OR\000\
  NEG\000\
  TRUE\000\
  FALSE\000\
  LPAR\000\
  RPAR\000\
  EOL\000\
  EOF\000\
  "

let yynames_block = "\
  EX\000\
  FA\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 27 "src/modelchecker/mucalcparser.mly"
                            ( _1 )
# 189 "src/modelchecker/mucalcparser.ml"
               : Mucalc.muexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 30 "src/modelchecker/mucalcparser.mly"
                            ( LFP (_2, _4) )
# 197 "src/modelchecker/mucalcparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 31 "src/modelchecker/mucalcparser.mly"
                            ( GFP (_2, _4) )
# 205 "src/modelchecker/mucalcparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 32 "src/modelchecker/mucalcparser.mly"
                            ( _2 )
# 212 "src/modelchecker/mucalcparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "src/modelchecker/mucalcparser.mly"
                            ( Con (_1, _3) )
# 220 "src/modelchecker/mucalcparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "src/modelchecker/mucalcparser.mly"
                            ( Dis (_1, _3) )
# 228 "src/modelchecker/mucalcparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 35 "src/modelchecker/mucalcparser.mly"
                            ( Neg _2 )
# 235 "src/modelchecker/mucalcparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "src/modelchecker/mucalcparser.mly"
                            ( Exists (_1, _2) )
# 243 "src/modelchecker/mucalcparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "src/modelchecker/mucalcparser.mly"
                            ( ForAll (_1, _2) )
# 251 "src/modelchecker/mucalcparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 38 "src/modelchecker/mucalcparser.mly"
                            ( Exists ("", _2) )
# 258 "src/modelchecker/mucalcparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 39 "src/modelchecker/mucalcparser.mly"
                            ( ForAll ("", _2) )
# 265 "src/modelchecker/mucalcparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 40 "src/modelchecker/mucalcparser.mly"
                            ( Var _1 )
# 272 "src/modelchecker/mucalcparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "src/modelchecker/mucalcparser.mly"
                            ( TT )
# 278 "src/modelchecker/mucalcparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "src/modelchecker/mucalcparser.mly"
                            ( FF )
# 284 "src/modelchecker/mucalcparser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Mucalc.muexpr)
