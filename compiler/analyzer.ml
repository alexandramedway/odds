(*
 * COMS4115: Semantic Analyzer
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

open Ast
open Sast
open Printf

(********************
 * Environment
 ********************)

module VarMap = Map.Make(String)

type environment = {
  params: Sast.var VarMap.t;
  scope: Sast.var VarMap.t;
}

(* Builtin variables and functions *)
let builtins = VarMap.empty
let builtins = VarMap.add "EUL" { name = "EUL"; s_type = Num; builtin = true; } builtins
let builtins = VarMap.add "PI" { name = "PI"; s_type = Num; builtin = true; } builtins

(* Core functions *)
let builtins = VarMap.add "exception" {
  name = "exception";
  s_type = Func({ param_types=[String]; return_type = Void; });
  builtin = true;
} builtins

let builtins = VarMap.add "print" {
  name = "print";
  s_type = Func({ param_types = [Any]; return_type = String; });
  builtin = true;
} builtins

(* Dist builtins *)
let builtins = VarMap.add "dist_add" {
  name = "dist_add";
  s_type = Func({ param_types = [Dist_t; Dist_t]; return_type = Dist_t; });
  builtin = true;
} builtins

let builtins = VarMap.add "dist_mult" {
  name = "dist_mult";
  s_type = Func({ param_types = [Dist_t; Dist_t]; return_type = Dist_t; });
  builtin = true;
} builtins

let builtins = VarMap.add "dist_shift" {
  name = "dist_shift";
  s_type = Func({ param_types = [Num; Dist_t]; return_type = Dist_t; });
  builtin = true;
} builtins

let builtins = VarMap.add "dist_stretch" {
  name = "dist_stretch";
  s_type = Func({ param_types = [Num; Dist_t]; return_type = Dist_t; });
  builtin = true;
} builtins

let builtins = VarMap.add "dist_exp" {
  name = "dist_exp";
  s_type = Func({ param_types = [Num; Dist_t]; return_type = Dist_t; });
  builtin = true;
} builtins

let builtins = VarMap.add "dist_sample" {
  name = "dist_sample";
  s_type = Func({ param_types = [Num; Dist_t]; return_type = List(Num); });
  builtin = true;
} builtins

let builtins = VarMap.add "P" {
  name = "P";
  s_type = Func({ param_types = [Num; Dist_t]; return_type = Num; });
  builtin = true;
} builtins

let builtins = VarMap.add "E" {
  name = "E";
  s_type = Func({ param_types = [Dist_t]; return_type = Num; });
  builtin = true;
} builtins

(* List builtins *)
let builtins = VarMap.add "head" {
  name = "head";
  s_type = Func({ param_types = [List(Any)]; return_type = Any; });
  builtin = true;
} builtins

let builtins = VarMap.add "tail" {
  name = "tail";
  s_type = Func({ param_types = [List(Any)]; return_type = List(Any); });
  builtin = true;
} builtins

let builtins = VarMap.add "cons" {
  name = "cons";
  s_type = Func({ param_types = [Any ; List(Any)]; return_type = List(Any); });
  builtin = true;
} builtins

let builtins = VarMap.add "len" {
  name = "len";
  s_type = Func({ param_types = [List(Any)]; return_type = Num; });
  builtin = true;
} builtins

let builtins = VarMap.add "concat" {
  name = "concat";
  s_type = Func({ param_types = [String; String]; return_type = String; });
  builtin = true;
} builtins


(* Program entry environment *)
let root_env = {
  params = VarMap.empty;
  scope = builtins;
}

(********************
 * Utilities
 ********************)

(* Given an ssid my_var_#, return the original key ID my_var *)
let id_of_ssid ssid =
  let id_len =
    try String.rindex ssid '_' with Not_found -> String.length ssid in
  String.sub ssid 0 id_len

let rec str_of_type = function
  | Num -> "Num"
  | String -> "String"
  | Bool -> "Bool"
  | Void -> "Void"
  | List(l) -> sprintf "List[%s]" (str_of_type l)
  | Func(f) -> str_of_func f
  | Dist_t -> "Dist"
  | Any -> "Any"
  | Unconst -> "Unconst"

and str_of_func f =
  let param_types = List.map str_of_type f.param_types and
    return_type = str_of_type f.return_type in
  sprintf "Func(%s => %s)" (String.concat ", " param_types) return_type

let str_of_unop = function
  | Not -> "!"      | Sub -> "-"

let str_of_binop = function
  (* Dist *)
  | D_Plus -> "<+>" | D_Times -> "<*>"
  | D_Shift -> "|+" | D_Stretch -> "|*"
  | D_Sample -> "<>"| D_Power -> "|**" 
  (* Arithmetic *)
  | Add -> "+"      | Sub -> "-"
  | Mult -> "*"     | Div -> "/"
  | Mod -> "%"      | Pow -> "**"
  (* Boolean *)
  | Or -> "||"      | And -> "&&"
  | Eq -> "=="      | Neq -> "!="
  | Less -> "<"     | Leq -> "<="
  | Greater -> ">"  | Geq -> ">="
  (* List *)
  | Cons -> "::"

let is_sugar = function
  | Cons | D_Plus | D_Times | D_Shift | D_Stretch | D_Power | D_Sample -> true
  | _ -> false 

let print_env env =
  let print_var id var =
    let line = sprintf "\t%s --> { name: %s; s_type: %s; }"
    id var.name (str_of_type var.s_type) in
    print_endline line in
  let str_of_varmap name vm =
    let header = sprintf "%s:" name in
    print_endline header; VarMap.iter print_var vm in
  print_endline "";
  str_of_varmap "env params" env.params;
  str_of_varmap "env scope" env.scope

(********************
 * Exceptions
 ********************)

exception Semantic_Error of string
exception Collect_Constraints_Error

let var_error id =
  let message = sprintf "Variable '%s' is undefined in current scope" id
  in raise (Semantic_Error message)

let unop_error op t = 
  let message = sprintf "Invalid use of unary operator '%s' with type %s"
    (str_of_unop op) (str_of_type t) in
  raise (Semantic_Error message)

let typ_mismatch t1 t2 = 
  let message = sprintf "Expected type %s but got type %s instead"
    (str_of_type t1) (str_of_type t2) in
  raise (Semantic_Error message)

let binop_error t1 op t2 = 
  let message =
    sprintf "Invalid use of binary operator '%s' with types %s and %s" 
    (str_of_binop op) (str_of_type t1) (str_of_type t2) in
  raise (Semantic_Error message)

let fcall_nonid_error () =
  let message = "Sast.Call provided non-ID as first argument" in
  raise (Semantic_Error message)

let fcall_nonfunc_error id typ =
  let id = match id with
    | Sast.Id(ssid) -> id_of_ssid ssid
    | _ -> fcall_nonid_error () in
  let message = sprintf "Attempting to call %s type '%s' as a function" 
    (str_of_type typ) id in
  raise (Semantic_Error message)

let fcall_length_error id num_params num_args =
  let name = match id with
    | Sast.Id(name) -> id_of_ssid name
    | _ -> fcall_nonid_error () in
  let message = sprintf
    "Function '%s' expects %d argument(s) but was called with %d instead"
    name num_params num_args in
  raise (Semantic_Error message)

let fcall_argtype_error id typ const =
  let name = match id with
    | Sast.Id(name) -> id_of_ssid name
    | _ -> fcall_nonid_error () in
  let message = sprintf
    "Function '%s' expected argument of type %s but was passed %s instead"
    name (str_of_type const) (str_of_type typ) in
  raise (Semantic_Error message)

let list_error list_type elem_type = 
  let message = sprintf "Invalid element of type %s in list of type %s"
    (str_of_type elem_type) (str_of_type list_type) in
  raise (Semantic_Error message)

let type_mismatch_error id typ const = 
  let message = sprintf 
    "Invalid usage of id '%s' with type %s when type %s was expected" 
    id (str_of_type typ) (str_of_type const) in
  raise (Semantic_Error message)

let return_type_mismatch_error func_id typ1 typ2 = 
  let message = sprintf "Invalid return type in function '%s':
    type '%s' expected to be returned, but type '%s' returned instead."
    func_id (str_of_type typ1) (str_of_type typ2) in
  raise (Semantic_Error message)

let fdecl_unconst_error id =
  let message = sprintf
    "Invalid declaration of function '%s' with unconstrained return value" id in
  raise (Semantic_Error message)

let fdecl_reassign_error id typ =
  let message = sprintf
    "Invalid attempt to reassign function identifier '%s' to type %s"
    id (str_of_type typ) in
  raise (Semantic_Error message)

let list_cons_mismatch_error typ const =
  let message = sprintf
    "Invalid attempt to prepend a value of type %s to list of type %s"
   (str_of_type typ) (str_of_type const) in
  raise (Semantic_Error message)

let constrain_error old_type const =
  let message = sprintf "Invalid attempt to change unconstrained type %s to %s"
    (str_of_type old_type) (str_of_type const) in
  raise (Semantic_Error message)

let if_mismatch_error typ1 typ2 =
  let message = sprintf
    "Invalid attempt to use conditional with mismatched types %s and %s"
    (str_of_type typ1) (str_of_type typ2) in
  raise (Semantic_Error message)

let dead_code_path_error ocaml_func_name =
  let message = 
    sprintf "ERROR: DEAD CODE PATH REACHED IN FUNCTION: %s" ocaml_func_name in
  raise (Semantic_Error message) 

let invalid_dist_min_max_error typ1 typ2 = 
  let message = sprintf "Invalid distribution with min type '%s' and max type '%s'" 
    (str_of_type typ1) (str_of_type typ2) in
  raise (Semantic_Error message)

let invalid_discr_dist_error typ1 typ2 = 
  let message = sprintf "Invalid distribution with vals type '%s' and weights type '%s'" 
    (str_of_type typ1) (str_of_type typ2) in
  raise (Semantic_Error message)

let invalid_dist_func_type_error invalid_typ f_typ =
  let message = sprintf "Invalid distribution with function '%s'
    (distribution's must have function of type '%s')" 
    (str_of_type invalid_typ) (str_of_type f_typ) in
  raise (Semantic_Error message)

let dead_code_path_error ocaml_func_name =
  let message = 
    sprintf "ERROR: DEAD CODE PATH REACHED IN FUNCTION: %s" ocaml_func_name in
  raise (Semantic_Error message) 

(********************
 * Scoping
 ********************)

(* Variable counter to prevent naming conflicts *)
let ss_counter = ref (-1)

(* Given a string x, get a unique id x_# to use as the next variable *)
let get_ssid name =
  ss_counter := !ss_counter + 1;
  sprintf "%s_%d" name !ss_counter

(* Add 'id' with type 's_type' to the environment scope *)
let add_to_scope env id s_type =
  let ss_id = get_ssid id in
  let var = { name = ss_id; s_type = s_type; builtin = false; } in
  let env' = {
    params = env.params;
    scope = VarMap.add id var env.scope;
  } in
  env', ss_id

(*
 * Add param with 'id' and type Unconst to the environment params, erasing it
 * from the environment scope
 *)
let add_to_params env id =
  let ss_id = get_ssid id in 
  let var = { name = ss_id; s_type = Unconst; builtin = false; } in
  let env' = {
    params = VarMap.add id var env.params;
    scope = VarMap.remove id env.scope;
  } in
  env', ss_id


(***********************************
 * Type inference and constraining
 ***********************************)

(* Update the type for given id corresponding to given 'ssid' in env *)
let update_type env ssid typ =
  let id = id_of_ssid ssid in
  if VarMap.mem id env.scope then (VarMap.find id env.scope).s_type <- typ else
  if VarMap.mem id env.params then (VarMap.find id env.params).s_type <- typ
  else var_error id

(* 
 * Attempt to constrain an ID in an expression one level down. E.g. !x would
 * constrain x to a boolean and x + y would constrain both x and y to integers,
 * but !(x == y) would not constrain either variable.
 *
 * Takes the current environment, type to constrain, and an expression wrapper
 * in which to search for an ID. Returns the newly constrained environment and
 * expression wrapper on success, or their old values on failure.
 *)
let rec constrain_ew env ew typ =
  let Sast.Expr(e, old_typ) = ew in
  if not (has_unconst old_typ) && old_typ <> typ then constrain_error old_typ typ else
  match e with
    | Sast.Id(ssid) -> update_type env ssid typ; env, Sast.Expr(e, typ)
    | Sast.Fdecl(f) -> update_type env f.f_name typ; env, Sast.Expr(e, typ)
    | Sast.Call(Sast.Expr(Sast.Id(ssid), Sast.Func(f)), _) ->
        let _, Sast.Expr(_, old_type) = check_id env (id_of_ssid ssid) in
        let old_ret_type = begin match old_type with
          | Sast.Func(old_f) -> old_f.return_type
          | _ as typ -> fcall_nonfunc_error (Sast.Id(ssid)) typ 
        end in
        if not (has_unconst f.return_type) && f.return_type <> old_ret_type then 
          constrain_error old_ret_type f.return_type
        else
          let f' = Func({ f with return_type = typ }) in
          update_type env ssid f'; env, Sast.Expr(e, f')
    | _ -> env, ew

(* This function is the same as constrain_ew, except instead of constraining
 * expression_wrappers, it constrains expressions. This function only modifies
 * the env and does not return an expression wrapper. 
 *)
and constrain_e env e typ = match e with
  | Sast.Id(ssid) -> update_type env ssid typ; env
  | _ -> env

(* This function takes 2 types. It returns 2 types. The first type returned 
 * will overwrite 'Any' to another type, including, possibly, 'Unconst.' The
 * second type returned will have 'Any' in it, overwriting any other type
 * when neccessary. TO DO: MAKE PRETTIER & SHORTER.
 *)
and collect_constraints typ1 typ2 = 
  (* Helper functions for collect_constraints *)
  let build_func collect_func func1 func2 = 
    let params1 = func1.param_types and params2 = func2.param_types and
      ret1 = func1.return_type and ret2 = func2.return_type in
    (* If different number of params, raise error *)
    let params' = 
      if List.length params1 <> List.length params2 then 
        raise Collect_Constraints_Error 
      else List.map2 collect_func params1 params2 and 
      ret' = collect_func ret1 ret2 in
    Func({ param_types = params'; return_type = ret'; })
  and build_list collect_func l_typ1 l_typ2 = 
    let l_typ' = collect_func l_typ1 l_typ2 in List(l_typ') in

  (* Collects possible constraints and returns type that is as constrained as 
   * possible. Any is always converted to Unconst. *)
  let rec overwrite_any typ1 typ2 = match typ1 with
    | Any | Unconst -> if typ2 <> Any then typ2 else Unconst
    | Func(func1) -> 
        begin match typ2 with
          | Any | Unconst -> typ1
          | Func(func2) -> build_func overwrite_any func1 func2
          | _ -> raise Collect_Constraints_Error
        end
    | List(l_typ1) -> 
        begin match typ2 with
          | Any | Unconst -> typ1
          | List(l_typ2) -> build_list overwrite_any l_typ1 l_typ2
          | _ -> raise Collect_Constraints_Error
        end
    | _  -> 
        if typ1 = typ2 || typ2 = Unconst || typ2 = Any then typ1
        else raise Collect_Constraints_Error

  (* Collects possible constraints and returns type that is as constrained as 
   * possible. Any remains. *)
  and keep_any typ1 typ2 = match typ1 with
    | Any -> Any
    | Unconst -> typ2
    | Func(func1) -> 
        begin match typ2 with
          | Any -> Any
          | Unconst -> typ1
          | Func(func2) -> build_func keep_any func1 func2
          | _ -> raise Collect_Constraints_Error
        end
    | List(l_typ1) ->
        begin match typ2 with
          | Any -> Any
          | Unconst -> typ1
          | List(l_typ2) -> build_list keep_any l_typ1 l_typ2
          | _ -> raise Collect_Constraints_Error
        end
    | _  -> 
        if typ2 = Any then Any
        else if typ1 = typ2 || typ2 = Unconst then typ1
        else raise Collect_Constraints_Error
  
  in overwrite_any typ1 typ2, keep_any typ1 typ2

(* Returns true if has Unconst, otherwise false *)
and has_unconst = function
  | Unconst -> true
  | List(l_typ) -> has_unconst l_typ
  | Func(func) -> 
    let is_param_unconst = List.map has_unconst func.param_types and
      is_ret_unconst = has_unconst func.return_type in
    List.mem true is_param_unconst || is_ret_unconst
  | _ -> false 

(* Turns Unconst types to Any *)
and unconst_to_any = function
  | Unconst -> Any
  | List(typ) -> let typ' = unconst_to_any typ in List(typ')
  | Func(func) -> 
      let param_types' = List.map unconst_to_any func.param_types and
        return_type' = unconst_to_any func.return_type in
      Func({ param_types = param_types'; return_type = return_type' })
  | _ as typ -> typ

(* Returns true if Num or Unconst, otherwise false *)
and is_num = function
  | Num | Unconst -> true
  | _ -> false 

and is_list_of_num = function
  | List(Num) | List(Unconst) -> true
  | _ -> false

(* Check list elements against constraint type, constrain if possible *)
and constrain_list_elems env acc const = function
  | [] -> env, Sast.Expr(Sast.Ldecl(List.rev acc), List(const))
  | (Sast.Expr(_, typ) as ew) :: tl ->
      let _, const_w_any = try collect_constraints typ const
        with
          | Collect_Constraints_Error -> list_error (List(const)) typ
          | _ as e -> raise e in
      (* MIGHT NEED TO CHECK IF UNCONST *)
      let env', ew' = constrain_ew env ew const_w_any in
      constrain_list_elems env' (ew' :: acc) const tl

(************************************************
 * Semantic checking and tree SAST construction
 ************************************************)

(* Branching point *)
and check_expr env = function
  | Ast.Num_lit(x) -> env, Sast.Expr(Sast.Num_lit(x), Num)
  | Ast.String_lit(s) -> env, Sast.Expr(Sast.String_lit(s), String)
  | Ast.Bool_lit(b) -> env, Sast.Expr(Sast.Bool_lit(b), Bool)
  | Ast.Void_lit -> env, Sast.Expr(Sast.Void_lit, Void)
  | Ast.Id(id) -> check_id env id
  | Ast.Unop(op, e) -> check_unop env op e
  | Ast.Binop(e1, op, e2) -> check_binop env e1 op e2
  | Ast.Call(id, args) -> check_func_call env id args
  | Ast.Assign(id, e) -> check_assign env id e
  | Ast.LDecl(l) -> check_list env l
  | Ast.Fdecl(f) -> check_fdecl env "_anon" f
  | Ast.Dist(d) -> check_dist env d
  | Ast.Discr_dist(d) -> check_discr_dist env d
  | Ast.Cake(fdecl, args) -> check_cake env fdecl args
  | Ast.If(i, t, e) -> check_if env i t e

(* Find string key 'id' in the environment if it exists *)
and check_id env id =
  let var =
    if VarMap.mem id env.scope then VarMap.find id env.scope else
    if VarMap.mem id env.params then VarMap.find id env.params else
    var_error id in
  env, Sast.Expr(Sast.Id(var.name), var.s_type)

(* Unary operators *)
and check_unop env op e =
  let env', ew = check_expr env e in
  let Sast.Expr(_, typ) = ew in
  match op with
    | Not -> begin match typ with
      | Bool -> env', Sast.Expr(Sast.Unop(op, ew), Bool)
      (* Attempt to constrain variable type of ew to Bool *)
      | Unconst -> let env', ew' = constrain_ew env' ew Bool in
          env', Sast.Expr(Sast.Unop(op, ew'), Bool)
      | _ as t -> unop_error op t
    end
    | Sub -> begin match typ with
      | Num -> env', Sast.Expr(Sast.Unop(op, ew), Num)
      (* Attempt to constrain variable type of ew to Num *)
      | Unconst -> let env', ew' = constrain_ew env' ew Num in
          env', Sast.Expr(Sast.Unop(op, ew'), Num)
      | _ as t -> unop_error op t
    end

(* Binary operators *)
and check_binop env e1 op e2 =
  if is_sugar op then (* Check if binop is sugar. If so unsugar. *)
    let func_name, e1', e2' = match op with
        | Cons -> "cons", e1, e2
        | D_Plus -> "dist_add", e1, e2
        | D_Times -> "dist_mult", e1, e2
        | D_Shift -> "dist_shift", e2, e1
        | D_Stretch -> "dist_stretch", e2, e1
        | D_Power -> "dist_exp", e2, e1
        | D_Sample -> "dist_sample", e2, e1
        | _ -> dead_code_path_error "check_binop" in
      
    (* Unsugar expression and refeed it to Analyzer *)
    let unsugared = Ast.Call(Ast.Id(func_name), [e1'; e2']) in
    check_expr env unsugared
  
  else (* binop is not sugar. Check, constrain, and proceed. *)
    let env', ew1 = check_expr env e1 in
    let Sast.Expr(_, typ1) = ew1 in
    let env', ew2 = check_expr env' e2 in
    let Sast.Expr(_, typ2) = ew2 in
    match op with
      (* Numeric operations *)
      | Add | Sub | Mult | Div | Mod | Pow | Less | Leq | Greater | Geq -> 
        if is_num typ1 && is_num typ2 then 
          let result_type = match op with
            | Add | Sub | Mult | Div | Mod | Pow -> Num
            | Less | Leq | Greater | Geq -> Bool
            | _ -> dead_code_path_error "check_binop" in
          (* Constrain variable types to Num if neccessary *)
          let env', ew1' = constrain_ew env' ew1 Num in
          let env', ew2' = constrain_ew env' ew2 Num in
          env', Sast.Expr(Sast.Binop(ew1', op, ew2'), result_type)
        else binop_error typ1 op typ2
      
      (* Equality operations - overloaded, no constraining can be done, can take
       * any type *)
      | Eq | Neq -> env', Sast.Expr(Sast.Binop(ew1, op, ew2), Bool)

      (* Boolean operations *)
      | Or | And ->
        let is_bool = function
          | Bool | Unconst -> true
          | _ -> false in
        if is_bool typ1 && is_bool typ2 then
          (* Constrain variable types to Bool if necessary *)
          let env', ew1' = constrain_ew env' ew1 Bool in
          let env', ew2' = constrain_ew env' ew2 Bool in
          env', Sast.Expr(Sast.Binop(ew1, op, ew2), Bool)
        else binop_error typ1 op typ2

      | _ -> dead_code_path_error "check_binop"

(* Function calling *)
and check_func_call env id args =
  let env', ew = check_expr env id in
  let Sast.Expr(id', typ) = ew in
  let env', ew', f = match typ with
    | Sast.Func(f) -> env', ew, f
    | Unconst -> 
        let f = {
          param_types = List.map (fun _ -> Unconst) args;
          return_type = Unconst;
        } in 
        let env', ew' = constrain_ew env' ew (Func(f)) in env', ew', f
    | _ -> fcall_nonfunc_error id' typ in
  let env', args = check_func_call_args env' id' f args in
  let env', ret_typ = check_func_call_ret env' id args f.return_type in
  let env', ew' = check_expr env' id in
  env', Sast.Expr(Sast.Call(ew', args), ret_typ)

and check_func_call_args env id f args =
  if List.length f.param_types <> List.length args then
    fcall_length_error id (List.length f.param_types) (List.length args) else
  let rec aux env acc acc_param_types param_types = function
    | [] -> env, List.rev acc, List.rev acc_param_types
    | e :: tl -> let env', ew = check_expr env e in
        let Sast.Expr(_, typ) = ew in
        let param_type = List.hd param_types in
        (* TO DO: What if user passes unconstrained variable to unconstrained function? *)
        let constrained, constrained_w_any = try collect_constraints typ param_type
          with
            | Collect_Constraints_Error -> fcall_argtype_error id typ param_type
            | _ as e -> raise e in
        let env', ew' = 
          (* NOTE: IF THINGS ARE ACTING WEIRD ERROR IS PROBABLY ON LINE BELOW *)
          if has_unconst typ && typ <> constrained then
            constrain_ew env ew constrained
          else env', ew in
        aux env' (ew' :: acc) (constrained_w_any :: acc_param_types) 
          (List.tl param_types) tl in
        
  let env', args', param_types' = aux env [] [] f.param_types args in
  
  if param_types' <> f.param_types then 
    let f_type = Func({ f with param_types = param_types'; }) in 
    let env' = constrain_e env' id f_type in 
    env', args'
  else env', args'

and check_func_call_ret env id args ret_default =
  let id' = match id with
    | Ast.Id(id') -> id'
    | _ -> dead_code_path_error "check_func_call_ret" in
  let is_builtin =
    if VarMap.mem id' env.scope then
      (VarMap.find id' env.scope).builtin
    else false in
  
  if not is_builtin then 
    (* If ret_default is Any, make it Unconst. Else if ret_default is a List
     * or contains lists of Any, make it List of Unconsts or List of Lists of 
     * Unconsts. *)
    let ret_default' = 
      let rec any_to_unconst_return = function
        | Any -> Unconst
        | List(typ) -> let typ' = any_to_unconst_return typ in List(typ')
        | _ as typ -> typ in
      any_to_unconst_return ret_default in
    env, ret_default'
  
  else (* is builtin *)
    match id' with
      | "head" -> let Sast.Expr(_, typ) = List.hd args in
          begin match typ with
            | List(t) -> env, t
            | _ -> dead_code_path_error "check_func_call_ret"
          end
      | "tail" -> let Sast.Expr(_, typ) = List.hd args in env, typ
      | "cons" ->
          let Sast.Expr(cons, c_typ) = List.hd args and
            Sast.Expr(l, l_typ) = List.hd (List.tl args) in
          let l_elem_typ = begin match l_typ with
            | List(t) -> t
            | _ -> dead_code_path_error "check_func_call_ret"
          end in
          let const, _ = try collect_constraints c_typ l_elem_typ
            with
              | Collect_Constraints_Error -> list_cons_mismatch_error c_typ l_typ
              | _ as e -> raise e in
          (* constrain the element begin appended *)
          let env' = constrain_e env cons const in
          (* constrain the list's type *)
          let env' = constrain_e env' l (List(const)) in
          env', List(const)
      | _ -> env, ret_default

(* Assignment *)
and check_assign env id = function
  | Ast.Fdecl(f) -> check_fdecl env id f
  | _ as e -> let env', ew = check_expr env e in
      let Sast.Expr(_, typ) = ew in
      let env', name = add_to_scope env' id typ in
      env', Sast.Expr(Sast.Assign(name, ew), typ)

(* Lists *)
and check_list env l =
  (* Evaluate list elements, transforming to sast types and storing list type *)
  let rec process_list env acc const = function
    | [] -> env, List.rev acc, const
    | e :: tl -> let env', ew = check_expr env e in
      let Sast.Expr(_, typ) = ew in
      let const', _ = try collect_constraints const typ
        with
          | Collect_Constraints_Error -> list_error (List(const)) typ
          | _ as e -> raise e in
      process_list env' (ew :: acc) const' tl in
  
  let env', l', const = process_list env [] Unconst l in
  constrain_list_elems env' [] const l'

(* Function declaration *)
and check_fdecl env id f =
  (* Add function name to scope with unconstrained param types and return type
   * to allow recursion *)
  let f_type = Func({
    param_types = List.map (fun _ -> Unconst) f.params;
    return_type = Unconst;
  }) in 
  
  (* Check if attempting to reassign an identifier belonging to the parent
   * function. If so, fail. If not, add the function to scope *)
  let env', name = 
    if VarMap.mem id env.scope then
      let old_type = (VarMap.find id env.scope).s_type in
      match old_type with
        | _ -> add_to_scope env id f_type
    else add_to_scope env id f_type in

  (* Evaluate parameters, body, and return statement in local environment *)
  let func_env, param_ssids = check_fdecl_params env' f.params in
  let func_env, body = check_stmts func_env f.body in
  let func_env, _ = check_expr func_env f.return in

  (* Evaluate parameter and function types. Check if the types of the 
   * parameters in the function type are the same as the types of the 
   * paramter variables themselves. If not, throw an error. Constrain Unconst 
   * paramters in both the function type and as variables where possible *)
  let rec check_params_type_mismatch env acc func_param_types = function
    | [] -> env, List.rev acc
    | ssid :: tl -> 
        let var = VarMap.find (id_of_ssid ssid) func_env.params and
          func_param_type = List.hd func_param_types in
        
        (* Constrain Param to extent possible *)
        let constrained, constrained_w_any = 
          try collect_constraints var.s_type func_param_type
          with 
            | Collect_Constraints_Error -> 
                type_mismatch_error id func_param_type var.s_type
            | _ as e -> raise e in

        (* Convert remaining Unconst to Any *)
        let constrained_w_any = unconst_to_any constrained_w_any in

        (* If constrained_param has constraints not present in var, then 
         * constrain var's type *)
        let func_env' = 
          if var.s_type <> constrained then
            constrain_e func_env (Sast.Id(ssid)) constrained
          else func_env in
        
        (* Recurse *)
        check_params_type_mismatch func_env' (constrained_w_any :: acc) 
          (List.tl func_param_types) tl in
        
  let param_types, return_typ =
    let f_typ = (VarMap.find id func_env.scope).s_type in
    match f_typ with
      | Func(func) -> func.param_types, func.return_type
      | _ -> fdecl_reassign_error id f_typ in
  let func_env, param_types' =
    check_params_type_mismatch func_env [] param_types param_ssids in
  
  (* Re-evaluate function return type to see if it has been constrained above *)
  let func_env, return = check_expr func_env f.return in

  (* If return type is Unconst, convert to Any *)
  let Sast.Expr(_, ret_type) = return in
  let ret_type' = unconst_to_any ret_type in
  
  (* If return type constrained differently than in env, throw error *)
  let rec not_any_and_not_unconst = function
    (* Returns false if type is Any, Unconst, or List of these. Otherwise
     * returns true *)
    | Any | Unconst -> false
    | List(typ) -> not_any_and_not_unconst typ
    | _ -> true in
  if not_any_and_not_unconst return_typ && ret_type' <> return_typ then
    return_type_mismatch_error id return_typ ret_type'
  else

  (* Construct function declaration *)
  let fdecl = {
    f_name = name;
    params = param_ssids;
    body = body;
    return = return;
  } in

  (* Construct function type *)
  let f_type = Func({ param_types = param_types'; return_type = ret_type' }) in
  
  (* Update function type in environment and return expression wrapper *)
  let ew = Sast.Expr(Sast.Fdecl(fdecl), Unconst) in
  (* MIGHT NEED TO CHECK IF UNCONST *)
  constrain_ew env' ew f_type

and check_fdecl_params env param_list =
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | param :: tl -> let env', name = add_to_params env param in
        aux env' (name :: acc) tl
  in aux env [] param_list

(* Caking *)
and check_cake env fdecl args =
  let env', fdecl_ew = check_expr env fdecl in
  let env', call_ew = check_func_call env' (Id("_anon")) args in
  let Sast.Expr(_, typ) = call_ew in
  env', Sast.Expr(Sast.Cake(fdecl_ew, call_ew), typ)

(* Conditionals *)
and check_if env i t e = 
  let env', ew1 = check_expr env i in
  let Sast.Expr(_, typ1) = ew1 in
  let env', ew1' = match typ1 with  
    | Unconst -> constrain_ew env' ew1 Bool 
    | Bool -> env, ew1
    | _ as typ -> typ_mismatch Bool typ in
  let env', ew2 = check_expr env' t in
  let Sast.Expr(_, typ2) = ew2 in
  let env', ew3 = check_expr env' e in
  let Sast.Expr(_, typ3) = ew3 in
  let const, _ = try collect_constraints typ2 typ3
    with
      | Collect_Constraints_Error -> if_mismatch_error typ2 typ3 
      | _ as e -> raise e in
  let env', ew2' = if has_unconst typ2 then constrain_ew env' ew2 const
    else env', ew2 in 
  let env', ew3' = if has_unconst typ3 then constrain_ew env' ew3 const 
    else env', ew3 in 
  let ifdecl = {
      c_name = (get_ssid "_cond");
      cond = ew1';
      stmt_1 = ew2';
      stmt_2 = ew3';
  } in
  env', Sast.Expr(Sast.If(ifdecl), const)

(* Distrubutions *)
and check_dist env d =
  (* Dists must have function of the following type: *)
  let dfunc_type = Func({ param_types = [Num]; return_type = Num; }) in

  (* Check and constrain min/max if neccessary *)
  let env', ew1 = check_expr env d.min in
  let Sast.Expr(_, typ1) = ew1 in
  let env', ew2 = check_expr env d.max in
  let Sast.Expr(_, typ2) = ew2 in
  let env', ew1', ew2' = 
    if is_num typ1 && is_num typ2 then
      let env', ew1' = constrain_ew env' ew1 Num in
      let env', ew2' = constrain_ew env' ew2 Num in
      env', ew1', ew2'
    else invalid_dist_min_max_error typ1 typ2 in

  (* Check and constrain distribution function *)
  let env', ew3 = check_expr env d.dist_func in
  let Sast.Expr(_, typ3) = ew3 in
  let const, _ = try collect_constraints typ3 dfunc_type
    with
      | Collect_Constraints_Error -> invalid_dist_func_type_error typ3 dfunc_type
      | _ as e -> raise e in
  let env', ew3' = 
    if has_unconst typ3 then constrain_ew env' ew3 const else env', ew3 in

  (* Construct Dist expr_wrapper *)
  let dist = Sast.Expr(Sast.Dist({ 
    min = ew1'; max = ew2'; dist_func = ew3';
  }), Dist_t) in
  
  (* Return Dist expr_wrapper *)
  env', dist

and check_discr_dist env d =
  (* Check and constrain min/max if neccessary *)
  let env', ew1 = check_expr env d.vals in
  let Sast.Expr(_, typ1) = ew1 in
  let env', ew2 = check_expr env d.weights in
  let Sast.Expr(_, typ2) = ew2 in
  let env', ew1', ew2' = 
    if is_list_of_num typ1 && is_list_of_num typ2 then
      let env', ew1' = constrain_ew env' ew1 (List(Num)) in
      let env', ew2' = constrain_ew env' ew2 (List(Num)) in
      env', ew1', ew2'
    else invalid_discr_dist_error typ1 typ2 in

  (* Construct Dist expr_wrapper *)
  let dist = Sast.Expr(Sast.Discr_dist({ 
    vals = ew1'; weights = ew2';
  }), Dist_t) in
  
  (* Return Dist expr_wrapper *)
  env', dist

(* Statements *)
and check_stmt env = function
  | Ast.Do(e) -> let env', ew = check_expr env e in env', Sast.Do(ew)

and check_stmts env stmt_list = 
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | stmt :: tl -> let env', e = check_stmt env stmt in
        aux env' (e :: acc) tl
  in aux env [] stmt_list

(* Program entry point *)
let check_ast ast = 
  let _, sast = check_stmts root_env ast in sast
