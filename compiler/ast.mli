(*
 * COMS4115: Odds abstract syntax tree
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

(* Unary operators *)
type unop =
  | Sub      (* - *) 
  | Not      (* ! *)

(* Binary operators *)
type binop =
  (* Dist *)
  | D_Plus     (* <+> *)
  | D_Times    (* <*> *)
  | D_Power    (* |** *)
  | D_Shift    (* |+ *)
  | D_Stretch  (* |* *)
  | D_Sample   (* <> *)
  (* Arithmetic *)
  | Add      (* + *)
  | Sub      (* - *)
  | Mult     (* * *)
  | Div      (* / *)
  | Mod      (* % *)
  | Pow      (* ** *)
  (* Boolean *)
  | Or       (* || *)
  | And      (* && *)
  | Eq       (* == *)
  | Neq      (* != *)
  | Less     (* < *)
  | Leq      (* <= *)
  | Greater  (* > *)
  | Geq      (* >= *)
  (* List *)
  | Cons     (* :: *)

(* Expressions *)
type num =
  | Num_int of int      (* 42 *)
  | Num_float of float  (* 42.0 *)

type expr =
  | Num_lit of num                (* 42 *)
  | String_lit of string          (* "Hello, world" *)
  | Bool_lit of bool              (* true *)
  | Void_lit                      (* void *)
  | Unop of unop * expr           (* -5 *)
  | Binop of expr * binop * expr  (* a + b *)
  | Id of string                  (* x *)
  | Assign of string * expr       (* x = 4 *)
  | Call of expr * expr list      (* add(1, 2) *)
  | LDecl of expr list            (* [1, 2, 3] *)
  | Dist of dist                  (* < 1, 2> | normal *) 
  | Discr_dist of discr_dist      (* |< l1, l2 >| *)
  | Fdecl of fdecl                (* (x) -> ... return x *)
  | Cake of expr * expr list      (* (() -> return 42)() *)
  | If of expr * expr * expr      (* if true then 42 else 43 *)

(* Distribution Declarations *)
and dist = {
  min: expr;        (* Distribution Minimum *)
  max: expr;        (* Distribution Maximum *)
  dist_func: expr;  (* Distribution Function *)
}

and discr_dist = {
  vals: expr;
  weights: expr;
}

(* Function Declarations *)
and fdecl = {
  params: string list;  (* Parameters *)
  body: stmt list;      (* Function Body *)
  return: expr;         (* Return *)
}
  
(* Statements *)
and stmt =
  | Do of expr     (* set foo = bar + 3 *)

(* Program entry point *)
type program = stmt list
