(*
 * COMS4115: Odds scanner
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

{ open Parser }

let num = ['0'-'9']
let whitespace = [' ' '\n' '\r']

rule token = parse
  
(* Whitespace *)
| whitespace*    { token lexbuf }

(* Comments *)
| "/*"    { comment lexbuf }

(* Function Symbols & Keywords *)
| ')' whitespace* "->"   { FDELIM }  | "return"   { RETURN }

(* Punctuation *)
| '('   { LPAREN }  | ')'   { RPAREN }
| '<'   { LCAR }    | '>'   { RCAR } (* Also relational operators *)
| '['   { LBRACE }  | ']'   { RBRACE }
| ','   { COMMA }   | '|'   { VBAR }

(* Arithmetic Operators *)
| '+'   { PLUS }    | '-'   { MINUS }
| '*'   { TIMES }   | '/'   { DIVIDE }
| '%'   { MOD }     | "**"  { POWER }

(* Relational Operators *)
| "=="    { EQ }    | "!="    { NEQ }
| "<="    { LEQ }   | ">="    { GEQ }

(* Logical Operators & Keywords*)
| "&&"    { AND }   | "||"    { OR }
| "!"     { NOT }

(* Assignment Operator *)
| '='   { ASN }

(* Conditional Operators *)
| "if"    { IF }    | "then"  { THEN }
| "else"  { ELSE }

(* Declarative Keywords *)
| "do"    { DO }

(* End-of-File *)
| eof { EOF }

(* Literals *)
| num+ as intlit { INT_LITERAL(int_of_string intlit) }
| num* '.' num+ as floatlit { FLOAT_LITERAL(float_of_string floatlit) }
| '"' (([^ '"'] | "\\\"")* as strlit) '"' { STRING_LITERAL(strlit) }
| "true" | "false" as boollit { BOOL_LITERAL(bool_of_string boollit)}
| "void" { VOID_LITERAL }

(* Identifiers *)
| ['a'-'z' 'A'-'Z' '_'] (['a'-'z' 'A'-'Z' '_' ] | num)* as lxm { ID(lxm) }

and comment = parse
| "*/"    { token lexbuf }
| _       { comment lexbuf }
