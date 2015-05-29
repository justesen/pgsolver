%{
open Ltl
%}

%token UNTIL NEXT
%token EVENTUALLY ALWAYS
%token AND OR NEG IMPL
%token <string> VAR 
%token TRUE FALSE
%token LPAR RPAR
%token EOL EOF

/* Precedence */
%right IMPL         /* lowest precedence */
%left AND OR
%left UNTIL NEXT
%right EVENTUALLY ALWAYS
%right NEG          /* highest precedence */

%start main         /* the entry point */
%type <Ltl.ltlexpr> main

%%

main:
    expr EOF                { $1 }
;
expr:
      LPAR expr RPAR        { $2 }
    | expr IMPL expr        { Dis (Neg $1, $3) }
    | expr AND expr         { Con ($1, $3) }
    | expr OR expr          { Dis ($1, $3) }
    | NEG expr              { Neg $2 }
    | NEXT expr             { Next $2 }
    | expr UNTIL expr       { Until ($1, $3) }
    | EVENTUALLY expr       { Eventually $2 }
    | ALWAYS expr           { Always $2 }
    | VAR                   { Var $1 }
    | TRUE                  { TT }
    | FALSE                 { FF }
;