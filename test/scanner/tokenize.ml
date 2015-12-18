open Parser
open Ast

type num =
  | Num_int of int
  | Num_float of float 

let stringify = function
  (* Punctuation *)
  | LPAREN -> "LPAREN"  | RPAREN -> "RPAREN"
  | LCAR -> "LCAR"      | RCAR -> "RCAR"
  | LBRACE -> "LBRACE"  | RBRACE -> "RBRACE"
  | COMMA -> "COMMA"    | VBAR -> "VBAR"
  | DDELIM -> "DDELIM"  | DISC -> "DISC"

  (* Dist Operators *)
  | DPLUS -> "DPLUS"    | DTIMES -> "DTIMES"
  | DPOWER -> "DPOWER"  | DSHIFT -> "DSHIFT"
  | DSTRETCH -> "DSTRETCH"

  (* Arithmetic Operators *)
  | PLUS -> "PLUS"     | MINUS -> "MINUS"
  | TIMES -> "TIMES"   | DIVIDE -> "DIVIDE"
  | MOD -> "MOD"       | POWER -> "POWER" 

  (* Relational Operators *)
  | EQ -> "EQ"    | NEQ -> "NEQ"
  | LEQ -> "LEQ"  | GEQ -> "GEQ"

  (* List Operators *)
  | CONS -> "CONS"

  (* Logical Operators & Keywords *)
  | AND -> "AND"   | OR -> "OR"
  | NOT -> "NOT"

  (* Assignment Operator *)
  | ASN -> "ASN"

  (* Conditional Operators *)
  | IF -> "IF"    | THEN -> "THEN"
  | ELSE -> "ELSE"

  (* Declarative Keywords *)
  | DO -> "DO"

  (* Function Symbols & Keywords *)
  | FDELIM -> "FDELIM"
  | RETURN -> "RETURN"
  | CAKE -> "CAKE"

  (* End-of-File *)
  | EOF -> "EOF"

  (* Identifiers *)
  | ID(string) -> "ID"

  (* Literals *)
  | NUM_LITERAL(num) -> "NUM_LITERAL"
  | STRING_LITERAL(string) -> "STRING_LITERAL"
  | BOOL_LITERAL(bool) -> "BOOL_LITERAL"
  | VOID_LITERAL -> "VOID_LITERAL"

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)
