(* equivalent to parser.mli when we write that *)
open Tokens

let stringify = function
  (* Punctuation *)
  | LPAREN -> "LPAREN"  | RPAREN -> "RPAREN"
  | LCAR -> "LCAR"      | RCAR -> "RCAR"
  | LBRACK -> "LBRACK"  | RBRACK -> "RBRACK"
  | SEMI -> "SEMI"      (*| COLON -> "COLON"*)
  | COMMA -> "COMMA"    | VBAR -> "VBAR"

  (* Arithmetic Operators *)
  | PLUS -> "PLUS"     | MINUS -> "MINUS"
  | TIMES -> "TIMES"   | DIVIDE -> "DIVIDE"
  | MOD -> "MOD"       | POWER -> "POWER" 

  (* Relational Operators *)
  | EQ -> "EQ"    | NEQ -> "NEQ"
  | LEQ -> "LEQ"  | GEQ -> "GEQ"

  (* Logical Operators & Keywords*)
  | AND -> "AND"   | OR -> "OR"
  | NOT -> "NOT"

  (* Assignment Operator *)
  | ASN -> "ASN"

  (* Conditional Operators *)
  | IF -> "IF"    | THEN -> "THEN"
  | ELSE -> "ELSE"

  (* Declarative Keywords *)
  | SET -> "SET"   | STATE -> "STATE"

  (* Distribution Keywords *)
  | NORM -> "NORM"    | BINOM -> "BINOM"
  | GAMMA -> "GAMMA"  | UNIFORM -> "UNIFORM"

  (* Function Symbols & Keywords *)
  | FDELIM -> "FDELIM"  (*| FRTYPE *)
  | RETURN -> "RETURN"

  (* End-of-File *)
  | EOF -> "EOF"

  (* Identifiers *)
  | ID(string) -> "ID"

  (* Literals *)
  | INT_LITERAL(int) -> "INT_LITERAL"
  | FLOAT_LITERAL(float) -> "FLOAT_LITERAL"
  | STRING_LITERAL(string) -> "STRING_LITERAL"
  | BOOL_LITERAL(bool) -> "BOOL_LITERAL"

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)
