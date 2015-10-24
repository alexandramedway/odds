type token =
(* Punctuation *)
| LPAREN  | RPAREN
| LCAR    | RCAR
| LBRACK  | RBRACK
| SEMI    (*| COLON *)
| COMMA   | VBAR

(* Arithmetic Operators *)
| PLUS    | MINUS
| TIMES   | DIVIDE
| MOD     | POWER

(* Relational Operators *)
| EQ    | NEQ
| LEQ   | GEQ

(* Logical Operators & Keywords*)
| AND   | OR
| NOT

(* Assignment Operator *)
| ASN

(* Conditional Operators *)
| IF    | THEN
| ELSE

(* Declarative Keywords *)
| SET   | STATE

(* Distribution Keywords *)
| NORM    | GAMMA
| BINOM   | UNIFORM

(* Function Symbols & Keywords *)
| FDELIM  (*| FRTYPE *)
| RETURN

(* End-of-File *)
| EOF

(* Identifiers *)
| ID of (string)

(* Literals *)
| INT_LITERAL of (int)
| FLOAT_LITERAL of (float)
| STRING_LITERAL of (string)
| BOOL_LITERAL of (bool)
