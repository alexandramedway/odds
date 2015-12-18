(*
 * COMS4115: Odds Python code generator
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

open Ast
open Past
open Printf

exception Python_Error of string

(* Indentation *)
let indent_of_num indent = String.make (4 * indent) ' '

(* Unary operators *)
let txt_of_unop = function
  | Not -> "not "
  | Sub -> "-"

(* Binary operators *)
let txt_of_num = function
  | Num_int(i) -> string_of_int i
  | Num_float(f) -> string_of_float f

let txt_of_binop = function
  (* Arithmetic *)
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"
  (* Boolean *)
  | Or -> "or"
  | And -> "and"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | _ -> raise (Python_Error "Unexpected binary operator")

(* Conditionals *)
let txt_of_cond indent i t e = sprintf "%sif %s:\n%s\n%s" 
  (indent_of_num indent) i t e

(* Expressions *)
let rec txt_of_expr = function
  | Num_lit(n) -> txt_of_num n
  | String_lit(s) -> sprintf "\"%s\"" s
  | Bool_lit(b) -> String.capitalize (string_of_bool(b))
  | None_lit -> "None"
  | Id(id) -> id
  | Unop(op, e) -> sprintf "(%s%s)" (txt_of_unop op) (txt_of_expr e)
  | Binop(e1, op, e2) -> sprintf "(%s %s %s)"
      (txt_of_expr e1) (txt_of_binop op) (txt_of_expr e2)
  | Call(id, args) -> txt_of_call id args
  | Ldecl(l) -> sprintf "[%s]" (txt_of_list l)

(* Function calls *)
and txt_of_call id args = match id with
  | Id("head") -> sprintf "%s[0]" (txt_of_expr (List.hd args))
  | Id("tail") -> sprintf "%s[1:]" (txt_of_expr (List.hd args))
  | Id("cons") ->
      let prepend = txt_of_expr (List.hd args) and
        list_txt = txt_of_expr (List.hd (List.tl args)) in
      sprintf "([%s] + %s)" prepend list_txt
  | _ -> sprintf "%s(%s)" (txt_of_expr id) (txt_of_list args)

(* Lists *)
and txt_of_list = function
  | [] -> ""
  | [x] -> txt_of_expr x
  | _ as l -> let strs = List.map (fun x -> txt_of_expr x) l in
    String.concat ", " strs

(* Functions *)
and txt_of_fdecl indent f =
    let params = String.concat ", " f.p_params in
    let body = txt_of_stmts (indent + 1) f.p_body in
    sprintf "%sdef %s(%s):\n%s"
      (indent_of_num indent)
      f.p_name
      params
      body

(* Statements *)
and txt_of_stmt indent = function 
  | Assign(id, e) -> sprintf "%s%s = %s" 
      (indent_of_num indent) id (txt_of_expr e)
  | Def(f) -> txt_of_fdecl indent f 
  | Return(e) -> sprintf "%sreturn %s" (indent_of_num indent) (txt_of_expr e)
  | If(i, t, e) -> 
      let i' = txt_of_expr i
        and t' = txt_of_stmt (indent + 1) t
        and e' = txt_of_stmt indent e in
      txt_of_cond indent i' t' e'
  | Stmt(e) -> sprintf "%s%s" (indent_of_num indent) (txt_of_expr e)

and txt_of_stmts indent stmt_list =
  let rec aux indent acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | stmt :: tl -> aux indent ((txt_of_stmt indent stmt) :: acc) tl
  in aux indent [] stmt_list

(* Code generation entry point *)
let gen_program past = txt_of_stmts 0 past
