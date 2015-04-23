%{
open Mucalc
%}

%token MU NU DOT
%token <string> EX
%token <string> FA
%token AEX AFA
%token AND OR NEG
%token <string> VAR 
%token TRUE FALSE
%token LPAR RPAR
%token EOL EOF

/* Precedence */
%nonassoc DOT       /* lowest precedence */
%left AND OR
%nonassoc EX FA AEX AFA
%nonassoc NEG       /* highest precedence */

%start main         /* the entry point */
%type <Mucalc.muexpr> main

%%

main:
    expr EOF                { $1 }
;
expr:
      MU VAR DOT expr       { LFP ($2, $4) }
    | NU VAR DOT expr       { GFP ($2, $4) }
    | LPAR expr RPAR        { $2 }
    | expr AND expr         { Con ($1, $3) }
    | expr OR expr          { Dis ($1, $3) }
    | NEG expr              { Neg $2 }
    | EX expr               { Exists ($1, $2) }
    | FA expr               { ForAll ($1, $2) }
    | AEX expr              { Exists ("", $2) }
    | AFA expr              { ForAll ("", $2) }
    | VAR                   { Var $1 }
    | TRUE                  { TT }
    | FALSE                 { FF }
;