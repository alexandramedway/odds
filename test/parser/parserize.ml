open Ast
open Printf

(* Unary operators *)
let txt_of_unop = function
  | Not -> "Not"
  | Sub -> "Sub"

(* Binary operators *)
let txt_of_binop = function
  (* Dist *)
  | D_Plus -> "D_Plus"
  | D_Times -> "D_Times"
  | D_Shift -> "D_Shift"
  | D_Stretch -> "D_Stretch"
  | D_Power -> "D_Power"
  | D_Sample -> "D_Sample"
  (* Arithmetic *)
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Div -> "Div"
  | Mod -> "Mod"
  | Pow -> "Pow"
  (* Boolean *)
  | Or -> "Or"
  | And -> "And"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Less -> "Less"
  | Leq -> "Leq"
  | Greater -> "Greater"
  | Geq -> "Geq"
  | Cons -> "::"

(* Expressions *)
let txt_of_num = function
  | Num_int(x) -> string_of_int x
  | Num_float(x) -> string_of_float x

let rec txt_of_expr = function
  | Num_lit(x) -> sprintf "Num_lit(%s)" (txt_of_num x)
  | String_lit(x) -> sprintf "String_lit(%s)" x
  | Bool_lit(x) -> sprintf "Bool_lit(%s)" (string_of_bool x)
  | Void_lit -> "Void_lit"
  | Id(x) -> sprintf "Id(%s)" x
  | Unop(op, e) -> sprintf "Unop(%s, %s)" (txt_of_unop op) (txt_of_expr e)
  | Binop(e1, op, e2) -> sprintf "Binop(%s, %s, %s)"
      (txt_of_expr e1) (txt_of_binop op) (txt_of_expr e2)
  | Call(f, args) -> sprintf "Call(%s, [%s])"
      (txt_of_expr f) (txt_of_list args)
  | Assign(x, e) -> sprintf "Assign(%s, %s)" x (txt_of_expr e)
  | LDecl(l) -> sprintf "LDecl([%s])" (txt_of_list l)
  | Dist(d) -> txt_of_dist d
  | Discr_dist(d) -> txt_of_discr_dist d
  | Fdecl(f)-> txt_of_fdecl f
  | Cake(fdecl, args) -> sprintf "Cake(%s, [%s])"
      (txt_of_expr fdecl) (txt_of_list args) 
  | If(e1, e2, e3) -> sprintf "If(%s, %s, %s)"
      (txt_of_expr e1) (txt_of_expr e2) (txt_of_expr e3)
  
and txt_of_dist d =
  sprintf "Dist({ min=%s ; max=%s ; dist_func=%s })"
    (txt_of_expr d.min) (txt_of_expr d.max) (txt_of_expr d.dist_func)

and txt_of_discr_dist d =
  sprintf "Dist({ vals=%s ; weights=%s })"
    (txt_of_expr d.vals) (txt_of_expr d.weights)

(* Function declarations *)
and txt_of_fdecl f =
  sprintf "Fdecl({ params=[%s] ; body=%s ; return = %s })"
    (String.concat " ; " f.params) (txt_of_stmts f.body) (txt_of_expr f.return)

(* Lists *)
and txt_of_list = function
  | [] -> ""
  | [x] -> txt_of_expr x
  | _ as l -> String.concat " ; " (List.map txt_of_expr l)

(* Statements *)
and txt_of_stmt = function
  | Do(expr) -> sprintf "Do(%s)" (txt_of_expr expr)

and txt_of_stmts stmts =
  let rec aux acc = function
      | [] -> sprintf "[%s]" (String.concat " ; " (List.rev acc))
      | stmt :: tl -> aux (txt_of_stmt stmt :: acc) tl
  in aux [] stmts

(* Program entry point *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let result = txt_of_stmts program in
  print_endline result
