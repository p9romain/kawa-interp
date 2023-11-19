open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let typecheck_prog p =
  let tenv = Hashtbl.fold (fun x t acc -> Env.add x t acc) p.globals Env.empty in

  let rec check e typ_expected tenv =
    let typ_e = type_expr e tenv in
    (* We allow x = null for anytime of x
       We check if they have different types
     *)
    if typ_e <> TVoid && typ_e <> typ_expected then
      begin
        match typ_e, typ_expected with
        (* Type casting allowed for int and float *)
        | TInt, TFloat
        | TFloat, TInt -> ()
         (* For inheritance : A extends B => B is also of type A *)
        | TClass cls_n, TClass cls_expected ->
          let rec check_inheritance_type class_name =
            if class_name <> cls_expected then
              let cl = Interpreter.get_class p.classes class_name in
                match cl.parent with
                | Some s_pt -> check_inheritance_type s_pt
                | None -> type_error typ_e typ_expected
          in
          check_inheritance_type cls_n
        | _ -> type_error typ_e typ_expected
      end

  and type_expr e tenv = 
    match e with
    | Int _ -> TInt      
    | Float _ -> TFloat
    | String _ -> TString
    | Bool _ -> TBool
    | Null -> TVoid
    | Unop (op, e) ->
      begin
        match op with
        | Opp ->
          begin
            match type_expr e tenv with
            | TInt -> TInt
            | TFloat -> TFloat
            | _ -> error "type error : the operator can be used on integers or floats"
          end
        | Not -> 
          begin
            match type_expr e tenv with
            | TBool -> TBool
            | _ -> error "type error : the operator can be used on booleans"
          end
      end
    | Binop (op, e1, e2) ->
      begin
        match op with
        | Add ->
           begin
            let t1 = type_expr e1 tenv in 
            let t2 = type_expr e2 tenv in
            match t1, t2 with
            | TInt, TInt -> TInt
            | TInt, TFloat
            | TFloat, TInt
            | TFloat, TFloat -> TFloat
            | TString, TString -> TString
            | _ -> error "type error : the operator can be used on integers, floats or strings"
          end
        | Sub 
        | Mul 
        | Div ->
          begin
            let t1 = type_expr e1 tenv in 
            let t2 = type_expr e2 tenv in
            match t1, t2 with
            | TInt, TInt -> TInt
            | TInt, TFloat
            | TFloat, TInt
            | TFloat, TFloat -> TFloat
            | _ -> error "type error : the operator can be used on integers or floats"
          end
        | Mod ->
          check e1 TInt tenv ;
          check e2 TInt tenv ;
          TInt
        | Le  
        | Lt  
        | Ge 
        | Gt ->
          begin
            let t1 = type_expr e1 tenv in 
            let t2 = type_expr e2 tenv in
            match t1, t2 with
            | TInt, TInt
            | TInt, TFloat
            | TFloat, TInt
            | TFloat, TFloat -> TBool
            | _ -> error "type error : the operator can be used on integers or floats"
          end
        | Eq  
        | Neq ->
          begin
            let t1 = type_expr e1 tenv in 
            let t2 = type_expr e2 tenv in
            match t1, t2 with
            | TInt, TInt
            | TInt, TFloat
            | TFloat, TInt
            | TFloat, TFloat
            | TString, TString
            | TBool, TBool -> TBool

            (* Can't check type if there is a null : this is let for the interpreter *)
            | _, TVoid
            | TVoid, _ -> TBool

            | TClass s1, TClass s2 -> 
              if s1 = s2 then
                TBool
              else
                type_error t1 t2

            | TInt, _
            | TFloat, _
            | TString, _
            | TBool, _
            | TClass _, _-> type_error t1 t2
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
    | InstanceOf (e, t) ->
      (* Check if [e] is well-typed *)
      let _ = type_expr e tenv in
      TBool
    | Get m -> type_mem_access m tenv
    | This ->
      begin
        match Env.find_opt "@This" tenv with
        | Some t -> t
        (* there is an issue : the interpreter manage this *)
        | None -> TVoid
      end
    | New s ->
      begin
        match Hashtbl.find_opt p.classes s with
        | Some _ -> TClass s
        | None -> TVoid
      end   
    | NewCstr (s, el) -> 
      let t = type_expr (New s) tenv in
      let () = check (MethCall(New s, s, el)) TVoid tenv in
      t
    | MethCall (e, s, el) ->
      begin
        match type_expr e tenv with
        | TClass c ->
          let cl = Interpreter.get_class p.classes c in
          match Hashtbl.find_opt cl.methods s with
          | Some m -> 
            (* Check if every expr in [el] is well-typed *)
            List.iter2 (fun e (_, t) -> check e t tenv) el m.params ;
            (* Create local environment with (in this order) :
               global + this + params (we already checked type) + local var of the method *)
            let tenv = Hashtbl.fold (fun x t acc -> Env.add x t acc) p.globals Env.empty in
            let tenv = Env.add "@This" (TClass c) tenv in
            let tenv = Hashtbl.fold (fun x t acc -> Env.add x t acc) m.locals tenv in
            let tenv = List.fold_left (fun acc (x, t) -> Env.add x t acc) tenv m.params 
            in
            (* check if method is well-typed *)
            check_seq m.code m.return tenv ;
            m.return
          | None -> TVoid
        | _ -> error "type error: can't access to a method of a non-class expression"
      end

  and type_mem_access m tenv = 
    match m with
    | Var s -> 
      begin
        match Env.find_opt s tenv with
        | Some t -> t
        | None -> TVoid 
      end
    | Field (e, attr) ->
      begin
        match type_expr e tenv with
        | TClass c ->
          let cl = Interpreter.get_class p.classes c in
          match Hashtbl.find_opt cl.attributes attr with
          | Some t -> t
          | None -> TVoid
        | _ -> error "type error: can't access to a field of a non-class expression"
      end
  (* change here from "in let rec" to "and" because I need it in type_expr (MethCall) *)
  and check_instr i ret tenv = 
    match i with
    (* Print implemented for any type *)
    | Print e ->
      (* Check if [e] is well-typed *)
      let _ = type_expr e tenv in 
      ()
    | Assert e ->
      check e TBool tenv
    | Set (m, s, e) ->
      begin
        (* Get [m], then do the binary operator [op] with e, and then check if it's the same type as [m]*)
        let op_then_set op = check (Binop(op, Get(m), e)) (type_mem_access m tenv) tenv
        in
        match s with
        | S_Set -> check e (type_mem_access m tenv) tenv
        | S_Add -> op_then_set Add
        | S_Sub -> op_then_set Sub
        | S_Mul -> op_then_set Mul
        | S_Div -> op_then_set Div
      end
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