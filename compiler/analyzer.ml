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

let builtins = VarMap.empty
let builtins = VarMap.add "EUL" { name = "EUL"; s_type = Num; } builtins
let builtins = VarMap.add "PI" { name = "PI"; s_type = Num; } builtins
let builtins = VarMap.add "print" {
  name = "print";
  s_type = Func({ param_types = [Any]; return_type = Void; });
} builtins

let root_env = {
  params = VarMap.empty;
  scope = builtins;
}


(********************
 * Utilities
 ********************)

(* Given an ssid my_var_#, return the original key ID my_var *)
let id_of_ssid ssid =
  let id_len = String.rindex ssid '_' in
  String.sub ssid 0 id_len

let rec str_of_type = function
  | Num -> "Num"
  | String -> "String"
  | Bool -> "Bool"
  | Void -> "Void"
  | List(l) -> sprintf "List[%s]" (str_of_type l)
  | Func(f) -> str_of_func f
  | Any -> "Any"
  | Unconst -> "Unconst"

and str_of_func f =
  let param_types = List.map str_of_type f.param_types and
    return_type = str_of_type f.return_type in
  sprintf "Func(%s => %s)" (String.concat ", " param_types) return_type

let str_of_unop = function
  | Not -> "!"      | Sub -> "-"

let str_of_binop = function
  (* Arithmetic *)
  | Add -> "+"      | Sub -> "-"
  | Mult -> "*"     | Div -> "/"
  | Mod -> "%"      | Pow -> "**"
  (* Boolean *)
  | Or -> "||"      | And -> "&&"
  | Eq -> "=="      | Neq -> "!="
  | Less -> "<"     | Leq -> "<="
  | Greater -> ">"  | Geq -> ">="

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

let bool_error t = 
  let message = sprintf "Expected type boolean, instead had type %s"
    (str_of_type t) in
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
  let message =
    sprintf "Function '%s' expects %d arguments but was called with %d instead"
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

let assign_error id typ =
  let message = sprintf "Invalid assignment of id '%s' to type %s"
    id (str_of_type typ) in
  raise (Semantic_Error message)

let list_error list_type elem_type = 
  let message = sprintf "Invalid element of type %s in list of type %s"
    (str_of_type elem_type) (str_of_type list_type) in
  raise (Semantic_Error message)

let recursive_type_mismatch_error f_id expected_type typ = 
  let message = sprintf 
    "Invalid recursive function call of function '%s' with type %s when type %s was expected" 
    f_id (str_of_type typ) (str_of_type expected_type) in
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

let constrain_error old_type const =
  let message = sprintf "Invalid attempt to change unconstrained type %s to %s"
    (str_of_type old_type) (str_of_type const) in
  raise (Semantic_Error message)

let constrain_if_error =
  let message = sprintf "Attempt to create conditional with two unconstrained outputs"
    in
  raise (Semantic_Error message)

let mismatch_if_error typ1 typ2 = 
  let message = sprintf "Invalid attempt to return two types from if, else of  %s & %s"
    (str_of_type typ1) (str_of_type typ2) in
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
  let var = { name = ss_id; s_type = s_type } in
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
  let var = { name = ss_id; s_type = Unconst } in
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
  if old_typ <> Unconst && old_typ <> typ then constrain_error old_typ typ else
  match e with
    | Sast.Id(ssid) -> update_type env ssid typ; env, Sast.Expr(e, typ)
    | Sast.Fdecl(f) -> update_type env f.fname typ; env, Sast.Expr(e, typ)
    | Sast.Call(Sast.Expr(Sast.Id(ssid), Sast.Func(f)), _) ->
        let _, Sast.Expr(_, old_type) = check_id env (id_of_ssid ssid) in
        let old_ret_type = begin match old_type with
          | Sast.Func(old_f) -> old_f.return_type
          | _ as typ -> fcall_nonfunc_error (Sast.Id(ssid)) typ 
        end in
        if f.return_type <> Unconst && f.return_type <> old_ret_type then 
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

(* Collects possible constraints and returns type that is as constrained as 
 * possible.
 *)
(* TODO: FIX THIS ERROR OCCURS WHEN ANY *)
and collect_constraints typ1 typ2 = match typ1 with
  | Unconst -> typ2
  | Func(func1) -> 
      begin match typ2 with
        | Unconst -> typ1
        | Func(func2) -> 
            let params1 = func1.param_types and params2 = func2.param_types and
              ret1 = func1.return_type and ret2 = func2.return_type in
            let params' = List.map2 collect_constraints params1 params2 and 
              ret' = collect_constraints ret1 ret2 in
            Func({ param_types = params'; return_type = ret'; })
        | _ -> raise Collect_Constraints_Error
      end
  | _  -> 
      if typ1 = typ2 || typ2 = Unconst then typ1 
      else raise Collect_Constraints_Error

(* Turns Unconst types to Any *)
and unconst_to_any = function
  | Unconst -> Any
  | Func(func) -> 
      let param_types' = List.map unconst_to_any func.param_types in
      Func({ func with param_types = param_types' })
  | _ as typ -> typ

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
  | Ast.List(l) -> check_list env l
  | Ast.Fdecl(f) -> check_fdecl env "anon" f true
  | Ast.If(e1, e2, e3) -> check_if env e1 e2 e3

(* Ensure e1 is a boolean, e2 and e3 are the same type *)
and check_if env e1 e2 e3 = 
  let env', ew1 = check_expr env e1 in
  let Sast.Expr(_, typ1) = ew1 in
  let env', ew1' = match typ1 with  
    | Unconst -> constrain_ew env' ew1 Bool 
    | Bool -> env, ew1
    | _ as t -> bool_error t in
  let env', ew2 = check_expr env' e2 in
  let Sast.Expr(_, typ2) = ew2 in
  let env', ew3 = check_expr env' e3 in
  let Sast.Expr(_, typ3) = ew3 in
  let const = try collect_constraints typ2 typ3
  with
    | Collect_Constraints_Error -> constrain_if_error 
    | _ as e -> raise e in
  let env', ew2' = constrain_ew env' ew2 const in
  let env', ew3' = constrain_ew env' ew3 const in 
  env', Sast.Expr(Sast.If(ew1, ew2', ew3'), const)

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
  let env', ew1 = check_expr env e1 in
  let Sast.Expr(_, typ1) = ew1 in
  let env', ew2 = check_expr env' e2 in
  let Sast.Expr(_, typ2) = ew2 in
  match op with
    
    (* Numeric operations *)
    | Add | Sub | Mult | Div | Mod | Pow | Less | Leq | Greater | Geq -> 
      let is_num = function
        | Num | Unconst -> true
        | _ -> false in 
      if is_num typ1 && is_num typ2 then 
        let result_type = match op with
          | Add | Sub | Mult | Div | Mod | Pow -> Num
          | Less | Leq | Greater | Geq -> Bool
          | _ -> binop_error typ1 op typ2 in
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
  let env', ew' = check_expr env' id in
  (* NEEDED TO ADD ABOVE LINE SO THAT ew' GETS UPDATED APPROPRIATELY 
   * ACCORDING TO CONSTRAINTS PLACED IN CHECK FUNC CALL ARGS - THIS IS HACKEY
   * LIKELY THERE IS A BETTER WAY OF DOING THIS, BUT PERHAPS NOT *)
  env', Sast.Expr(Sast.Call(ew', args), f.return_type)

and check_func_call_args env id f args =
  if List.length f.param_types <> List.length args then
    fcall_length_error id (List.length f.param_types) (List.length args) else
  let rec aux env acc acc_param_types param_types = function
    | [] -> env, List.rev acc, List.rev acc_param_types
    | e :: tl -> let env', ew = check_expr env e in
        let Sast.Expr(_, typ) = ew in
        let param_type = List.hd param_types in
        if typ = param_type || param_type = Any then
          aux env' (ew :: acc) (param_type :: acc_param_types) (List.tl param_types) tl
        (* TO DO: What if user passes unconstrained variable to unconstrained function? *)
        else
          let constrained_param = try collect_constraints typ param_type
            with
              | Collect_Constraints_Error -> fcall_argtype_error id typ param_type
              | _ as e -> raise e in
          let env', ew' = 
            if typ <> constrained_param then
              constrain_ew env ew constrained_param
            else env', ew in
          aux env' (ew' :: acc) (constrained_param :: acc_param_types) 
            (List.tl param_types) tl in
        
  let env', args', param_types' = aux env [] [] f.param_types args in
  
  if param_types' <> f.param_types then 
    let f_type = Func({ f with param_types = param_types'; }) in 
    let env' = constrain_e env' id f_type in 
    env', args'
  else env', args'

(* Assignment *)
and check_assign env id = function
  | Ast.Fdecl(f) -> check_fdecl env id f false
  | _ as e -> let env', ew = check_expr env e in
      let Sast.Expr(_, typ) = ew in
      if typ = Void then assign_error id Void else
      let env', name = add_to_scope env' id typ in
      env', Sast.Expr(Sast.Assign(name, ew), typ)

(* Lists *)
and check_list env l =
  (* Evaluate list elements, transforming to sast types and storing list type *)
  let rec process_list env acc const = function
    | [] -> env, List.rev acc, const
    | e :: tl -> let env', ew = check_expr env e in
      if const <> Unconst then process_list env' (ew :: acc) const tl else
      let Sast.Expr(_, typ) = ew in process_list env' (ew :: acc) typ tl in
  let env', l', const = process_list env [] Unconst l in
  
  (* Check list elements against constraint type, constrain if possible *)
  let rec check_list_elems env acc = function
    | [] -> env, Sast.Expr(Sast.List(List.rev acc), List(const))
    | (Sast.Expr(_, typ) as ew) :: tl ->
      if typ = const || const = Unconst then check_list_elems env (ew :: acc) tl
      else if typ = Unconst then
        let env', ew' = constrain_ew env ew const in
        check_list_elems env' (ew' :: acc) tl
      else list_error (List(const)) typ in
  check_list_elems env' [] l'

(* Function declaration *)
and check_fdecl env id f anon =
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
        | Func(f) when f.return_type = Unconst -> fdecl_reassign_error id f_type
        | _ -> add_to_scope env id f_type
    else add_to_scope env id f_type in

  (* Evaluate parameters, body, and return statement in local environment *)
  let func_env, param_ssids = check_fdecl_params env' f.params in
  let func_env, body = check_stmts func_env f.body in
  let func_env, _ = check_expr func_env f.return in

  (* Evaluate parameter and function types. Check if the types of the 
   * parameters in the function's type are the same as the types of the 
   * paramter variables themselves. If not, throw an error. Constrain Unconst 
   * paramters - in both the function's type and as variables - where possible *)
  let rec check_params_type_mismatch env acc func_param_types = function
    | [] -> env, List.rev acc
    | ssid :: tl -> 
        let var = VarMap.find (id_of_ssid ssid) func_env.params and
          func_param_type = List.hd func_param_types in
        
        (* Constrain Param to extent possible *)
        let constrained_param = 
          try collect_constraints var.s_type func_param_type
          with 
            | Collect_Constraints_Error -> 
                recursive_type_mismatch_error id var.s_type func_param_type
            | _ as e -> raise e in

        (* Convert remaining Unconst to Any *)
        let constrained_param' = unconst_to_any constrained_param in

        (* If constrained_param has constraints not present in var, then 
         * constrain var's type *)
        let func_env' = 
          if var.s_type <> constrained_param' then
            constrain_e func_env (Sast.Id(ssid)) constrained_param'
          else func_env in
        
        (* Recurse *)
        check_params_type_mismatch func_env' (constrained_param' :: acc) 
          (List.tl func_param_types) tl in
        
  let param_types = 
    let typ = (VarMap.find id func_env.scope).s_type in
    match typ with
      | Func(func) -> func.param_types
      | _ -> fdecl_reassign_error id typ in
  let func_env, param_types' = check_params_type_mismatch func_env [] param_types param_ssids in
  
  (* Re-evaluate function return type to see if it has been constrained above *)
  let func_env, return = check_expr func_env f.return in

  (* Unconstrained function return types are not allowed *)
  let Sast.Expr(_, ret_type) = return in
  if ret_type = Any || ret_type = List(Unconst) then 
    fdecl_unconst_error id 
  else

  (* Construct function declaration *)
  let fdecl = {
    fname = name;
    params = param_ssids;
    body = body;
    return = return;
    is_anon = anon;
  } in

  (* Construct function type *)
  let f_type = Func({ param_types = param_types'; return_type = ret_type }) in
  
  (* Update function type in environment and return expression wrapper *)
  let ew = Sast.Expr(Sast.Fdecl(fdecl), Unconst) in
  constrain_ew env' ew f_type

and check_fdecl_params env param_list =
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | param :: tl -> let env', name = add_to_params env param in
        aux env' (name :: acc) tl
  in aux env [] param_list

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
