open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let rec check e typ_expected tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> TVoid && typ_e <> typ_expected then
      begin
        match typ_e with
        | TClass cls_n -> (* for inheritance : A extends B => B is also of type A *)
          let rec check_inheritance_type class_name =
            if TClass(class_name) <> typ_expected then
              begin
                match List.find_opt (fun cl -> cl.class_name = class_name) p.classes with
                | Some cl ->
                    begin
                      match cl.parent with
                      | Some s_pt -> check_inheritance_type s_pt
                      | None -> type_error typ_e typ_expected
                    end
                | None -> error ("unbound value error: '" ^ class_name ^ "' class is not declared in the program.")
              end 
          in
          check_inheritance_type cls_n
        | _ -> type_error typ_e typ_expected
      end

  and type_expr e tenv = 
    match e with
    | Int _ -> TInt      
    | Bool _ -> TBool     
    | Null -> TVoid
    | Unop (op, e) ->
      begin
        match op with
        | Opp -> TInt
        | Not -> TBool
      end
    | Binop (op, e1, e2) ->
      begin
        match op with
        | Add
        | Sub 
        | Mul 
        | Div 
        | Mod ->
          check e1 TInt tenv ;
          check e2 TInt tenv ;
          TInt
        | Le  
        | Lt  
        | Ge 
        | Gt ->
          check e1 TInt tenv ;
          check e2 TInt tenv ;
          TBool
        | Eq  
        | Neq ->
          begin
            let t1 = type_expr e1 tenv in 
            let t2 = type_expr e2 tenv in
            match t1, t2 with
            | TInt, TInt
            | TVoid, TVoid
            | TBool, TBool -> TBool
            | TClass s1, TClass s2 -> 
              if s1 = s2 then
                TBool
              else
                type_error t1 t2
            | TInt, _
            | TBool, _
            | TClass _, _-> type_error t1 t2
            | _, _ -> error "type error : can't compare types who aren't integers, booleans or objects."
          end
        | And 
        | Or ->
            check e1 TBool tenv ;
            check e2 TBool tenv ;
            TBool
      end 
    | TerCond (t, e1, e2) ->
      let () = check t TBool tenv in
      let t1 = type_expr e1 tenv in 
      let t2 = type_expr e2 tenv in
      if not (t1 = TVoid || t2 = TVoid) && t1 <> t2 then 
       type_error t1 t2
      else
        if t1 = t2 then
          t1
        else
          (* t1 or t2 are void *)
          TVoid
    | Get m -> type_mem_access m tenv
    | This ->
      begin
        match Env.find_opt "@This" tenv with
        | Some t -> t
        | None -> error "unbound value error: can't access to 'this'.\nHint : are you inside a class ?"
      end
    | New s ->
      begin
        match List.find_opt (fun cl -> cl.class_name = s) p.classes with
        | Some _ -> TClass s
        | None -> error ("unbound value error: '" ^ s ^ "' class is not declared in the program.")
      end   
    | NewCstr (s, el) -> 
      let t = type_expr (New s) tenv in
      let () = check (MethCall(New s, "constructor", el)) TVoid tenv in
      t
    | MethCall (e, s, el) ->
      begin
        match type_expr e tenv with
        | TClass c ->
          begin
            match List.find_opt (fun cl -> cl.class_name = c) p.classes with
            | Some cl -> 
              begin
                match List.find_opt (fun m -> m.method_name = s ) cl.methods with
                | Some m -> 
                  List.iter2 (fun e (_, t) -> check e t tenv) el m.params ;
                  (* Create local environment with :
                     global + this + params (we already checked type) + local var of the method *)
                  let tenv = add_env ([("@This", TClass c)] @ m.params @ m.locals @ p.globals) Env.empty in
                  (* check if method is well-typed *)
                  check_seq m.code m.return tenv ;
                  m.return
                | None -> error ("unbound value error: can't acces the method '" ^ s 
                           ^ "' in the object of class '" ^ c ^ "'.")
              end
            | None -> error ("unbound value error: '" ^ c ^ "' class is not declared in the program.")
          end
        | _ -> error "type error: can't access to a method of a non-class expression"
      end

  and type_mem_access m tenv = 
    match m with
    | Var s -> 
      begin
        match Env.find_opt s tenv with
        | Some t -> t
        | None -> error ("unbound value error: '" ^ s ^ "' is not declared in the scope.")
      end
    | Field (e, attr) ->
      begin
        match type_expr e tenv with
        | TClass c ->
          begin
            match List.find_opt (fun cl -> cl.class_name = c) p.classes with
            | Some cl -> 
              begin
                match List.find_opt (fun (x, _) -> x = attr ) cl.attributes with
                | Some (_, t) -> t
                | None -> error ("unbound value error: can't acces the field '" ^ attr 
                           ^ "' in the object of class '" ^ c ^ "'.")
              end
            | None -> error ("unbound value error: '" ^ c ^ "' class is not declared in the program.")
          end
        | _ -> error "type error: can't access to a field of a non-class expression"
      end
  (* change here from "in let rec" to "and" because I need it in type_expr (MethCall) *)
  and check_instr i ret tenv = 
    match i with
    | Print e -> (* Print implemented for any type *)
      let _ = type_expr e tenv in 
      () 
    | Set (m, e) ->
      check e (type_mem_access m tenv) tenv
    | Cond c -> check_cond c ret tenv
    | While (e, s) ->
      check e TBool tenv ;
      check_seq s ret tenv ;
    | DoWhile (s, w) ->
      check_seq s ret tenv ;
      check_instr w ret tenv ;
    | Return e ->
      check e ret tenv ;
    | Expr e ->
      check e TVoid tenv ;
  and check_cond c ret tenv =
    match c with
    | If (e, s) ->
      check e TBool tenv ;
      check_seq s ret tenv ;
    | If_Else (e, s, c) ->
      check e TBool tenv ;
      check_seq s ret tenv ;
      check_cond c ret tenv ;
    | Else s ->
      check_seq s ret tenv ;
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in

  check_seq p.main TVoid tenv