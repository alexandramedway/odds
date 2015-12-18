/*
 * COMS4115: Odds parser
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 */

%{ open Ast %}

/* Punctuation */
%token LPAREN RPAREN LCAR RCAR LBRACE RBRACE COMMA VBAR DDELIM DISC

/* Arithmetic Operators */
%token PLUS MINUS TIMES DIVIDE MOD POWER DPLUS DTIMES DPOWER DSHIFT DSTRETCH

/* List Operators */
%token CONS

/* Relational Operators */
%token EQ NEQ LEQ GEQ

/* Logical Operators & Keywords*/
%token AND OR NOT

/* Assignment Operator */
%token ASN

/* Conditional Operators */
%token IF THEN ELSE

/* Declarative Keywords */
%token DO

/* Function Symbols & Keywords */
%token FDELIM RETURN CAKE

/* End-of-File */
%token EOF

/* Identifiers */
%token <string> ID

/* Literals */
%token <Ast.num> NUM_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOL_LITERAL
%token VOID_LITERAL

/* Precedence and associativity of each operator */
%nonassoc ELSE
%nonassoc RETURN
%right ASN
%left CONS
%left OR
%left AND
%left EQ NEQ
%left LCAR LEQ RCAR GEQ
%left DSHIFT DSTRETCH
%left PLUS MINUS DPLUS
%left TIMES DIVIDE MOD DTIMES
%left POWER DPOWER
%right NOT

%start program                /* Start symbol */
%type <Ast.program> program   /* Type returned by a program */

%%

/* Program flow */
program:
  | stmt_list EOF                         { List.rev $1 }

stmt_list:
  | /* nothing */                         { [] }
  | stmt_list stmt                        { $2 :: $1 }

stmt:
  | DO expr                               { Do($2) }

/* Expressions */
expr:
  | literal                               { $1 }
  | arith_ops                             { $1 }
  | bool_ops                              { $1 }
  | list_ops                              { $1 }
  | dist_ops                              { $1 }
  | dist                                  { Dist($1) }
  | discr_dist                            { Discr_dist($1) }
  | ID                                    { Id($1) }
  | ID ASN expr                           { Assign($1, $3) }
  | ID LPAREN list_opt RPAREN             { Call(Id($1), $3) }
  | LBRACE list_opt RBRACE                { LDecl($2) }
  | LPAREN expr RPAREN                    { $2 }
  | fdecl                                 { Fdecl($1) }
  | LPAREN fdecl CAKE list_opt RPAREN     { Cake(Fdecl($2), $4) }
  | IF expr THEN expr ELSE expr           { If($2, $4, $6) }

/* Function declaration */
fdecl:
  | LPAREN fparam_opt FDELIM stmt_list RETURN expr
    { {
      params = $2;
      body = List.rev $4;
      return = $6;
    } }

fparam_opt:
  | /* nothing */                         { [] }
  | fparam_list                           { List.rev $1 }

fparam_list:
  | ID                                    { [$1] }
  | fparam_list COMMA ID                  { $3 :: $1 }

/* Lists and function calling */
list_opt:
  | /* nothing */                         { [] }
  | list                                  { List.rev $1 }

list:
  | expr                                  { [$1] }
  | list COMMA expr                       { $3 :: $1 }

/* Distributions */
dist:
  | LCAR expr COMMA expr DDELIM expr VBAR
    { {
      min = $2;
      max = $4;
      dist_func = $6;
    } }

discr_dist:
  | DISC expr COMMA expr DDELIM 
    { {
      vals = $2;
      weights = $4;
    } }

/* Binary operators */
arith_ops:
  | MINUS expr                            { Unop(Sub, $2) }
  | expr PLUS expr                        { Binop($1, Add, $3) }
  | expr MINUS expr                       { Binop($1, Sub, $3) }
  | expr TIMES expr                       { Binop($1, Mult, $3) }
  | expr DIVIDE expr                      { Binop($1, Div, $3) }
  | expr MOD expr                         { Binop($1, Mod, $3) }
  | expr POWER expr                       { Binop($1, Pow, $3) }

bool_ops:
  | NOT expr                              { Unop(Not, $2) }
  | expr OR expr                          { Binop($1, Or, $3) }
  | expr AND expr                         { Binop($1, And, $3) }
  | expr EQ expr                          { Binop($1, Eq, $3) }
  | expr NEQ expr                         { Binop($1, Neq, $3) }
  | expr LCAR expr                        { Binop($1, Less, $3) }
  | expr LEQ expr                         { Binop($1, Leq, $3) }
  | expr RCAR expr                        { Binop($1, Greater, $3) }
  | expr GEQ expr                         { Binop($1, Geq, $3) }

dist_ops:
  | expr DPLUS expr                       { Binop($1, D_Plus, $3) }
  | expr DTIMES expr                      { Binop($1, D_Times, $3) }
  | expr DPOWER expr                      { Binop($1, D_Power, $3) }
  | expr DSHIFT expr                      { Binop($1, D_Shift, $3) }
  | expr DSTRETCH expr                    { Binop($1, D_Stretch, $3) }
  | expr LCAR RCAR expr                   { Binop($1, D_Sample, $4)}

list_ops:
  | expr CONS expr                        { Binop($1, Cons, $3) }

/* Literals */
literal:
  | NUM_LITERAL                           { Num_lit($1) }
  | STRING_LITERAL                        { String_lit($1) }
  | BOOL_LITERAL                          { Bool_lit($1) }
  | VOID_LITERAL                          { Void_lit }
