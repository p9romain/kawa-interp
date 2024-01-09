open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error @@ 
    Printf.sprintf "expected %s, got %s" (typ_to_string ty_expected) @@ typ_to_string ty_actual

module Env = Map.Make(String)

let typecheck_prog (p : program) : unit =
  let tenv = Hashtbl.fold (fun x t acc -> Env.add x t acc) p.globals Env.empty in

  let rec check (e : expr) 
                (typ_expected : typ) 
                (tenv : typ Env.t) : unit =
    let typ_e = type_expr e tenv in
    (* We allow x = null for anytime of x
       We check if they have different types
     *)
    if typ_e <> TVoid && typ_e <> typ_expected then
      begin
        match typ_e, typ_expected with
        (* Type casting allowed for int and float *)
        | TInt, TFloat -> ()
        | TChar, TString -> ()
         (* For inheritance : A extends B => B is also of type A *)
        | TClass cls_n, TClass cls_expected ->
          let rec check_inheritance_type (class_name : string) : unit =
            if class_name <> cls_expected then
              let cl = Interpreter.get_class p.classes class_name in
                match cl.parent with
                | Some s_pt -> check_inheritance_type s_pt
                | None -> type_error typ_e typ_expected
          in
          check_inheritance_type cls_n
        | _ -> type_error typ_e typ_expected
      end

  and type_expr (e : expr) 
                (tenv : typ Env.t) : typ = 
    match e with
    | Int _ -> TInt   
    | IntCast e ->
      begin
        match type_expr e tenv with
        | TInt   
        | TFloat -> TInt
        | _ -> error "type error : the int casting can only be used on integers or floats"
      end   
    | Float _ -> TFloat   
    | FloatCast e ->
      begin
        match type_expr e tenv with
        | TInt   
        | TFloat -> TFloat
        | _ -> error "type error : the int casting can only be used on integers or floats"
      end
    | Char _ -> TChar
    | String _ -> TString  
    | StringCast e -> 
      (* check if [e] is well-typed *)
      let _ = type_expr e tenv in
      TString
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
            | TChar, TChar
            | TChar, TString
            | TString, TChar
            | TString, TString -> TString
            | _ -> error "type error : the + operator can only be used on integers or floats, or on characters or strings"
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
            | _ -> 
              let op_to_string op =
                match op with
                | Sub -> "-"
                | Mul -> "*"
                | Div -> "/"
                | _ -> ""
              in
              error @@ "type error : the " ^ (op_to_string op) ^ " operator can only be used on integers or floats"
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
            | _ -> error "type error : the comparison operator can only be used on integers or floats"
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
            | TChar, TChar
            | TChar, TString
            | TString, TChar
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
            | TChar, _
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
      let () = check (MethCall (New s, s, el)) TVoid tenv in
      t
    | MethCall (e, s, el) ->
      begin
        match type_expr e tenv with
        | TClass c ->
          let cl = Interpreter.get_class p.classes c in
          match Hashtbl.find_opt cl.methods @@ Interpreter.method_name_type s @@ List.map (fun e -> type_expr e tenv) el with
          | Some m -> 
            (* Code already checked (function unused) : check_meth cl (Some el) m ; *)
            (* Check if every expr in [el] is well-typed *)
            List.iter2 (fun e (_, t) -> check e t tenv) el m.params ;
            m.return
          | None -> TVoid (* Interpreter role *)
        | _ -> error "type error: can't access to a method of a non-class expression"
      end

  and type_mem_access (m : mem_access)
                      (tenv : typ Env.t) : typ = 
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
  and check_instr (i : instr) 
                  (ret : typ)
                  (tenv : typ Env.t) : unit = 
    match i with
    (* Print implemented for any type *)
    | Print e ->
      (* Check if [e] is well-typed *)
      let _ = type_expr e tenv in 
      ()
    | Println e ->
      (* Check if [e] is well-typed *)
      let _ = type_expr e tenv in 
      ()
    | Input (s, e) ->
      begin
        let _ = 
          match s with
          | None -> ()
          | Some s ->
            (* Check if [s] is well-typed *)
            let _ = check s TString tenv in
            ()
        in
        (* Check if [e] is well-typed *)
        match type_expr e tenv with
        | TClass _
        | TBool
        | TVoid -> error "type error: variable need to be an integer, a float or a string"
        | _ -> ()
      end
    | Assert e ->
      check e TBool tenv
    | Set (m, s, e) ->
      begin
        (* Get [m], then do the binary operator [op] with e, and then check if it's the same type as [m]*)
        let op_then_set op = check (Binop (op, Get m, e)) (type_mem_access m tenv) tenv
        in
        match s with
        | S_Set -> check e (type_mem_access m tenv) tenv
        | S_Add -> op_then_set Add
        | S_Sub -> op_then_set Sub
        | S_Mul -> op_then_set Mul
        | S_Div -> op_then_set Div
      end
    | Cond c -> check_cond c ret tenv
    | For (t, set, cond, incr, seq) ->
      begin
        let tenv =
          match t with
          | None -> tenv (* we use an external variable in the loop (or we do something else) *)
          | Some t -> (* we need to create a variable *)
            let Set (Var var, _, _) = set in
            let tenv = Env.add var t tenv in
            tenv
        in
        check_instr set TVoid tenv ;
        check cond TBool tenv ;
        check_instr incr TVoid tenv ;
        check_seq seq ret tenv ;
      end
    | While (e, s) ->
      check e TBool tenv ;
      check_seq s ret tenv
    | DoWhile (s, w) ->
      check_seq s ret tenv ;
      check_instr w ret tenv
    | Return e ->
      begin
        match e with
        | None -> check Null ret tenv
        | Some e -> check e ret tenv
      end
    | Expr e ->
      check e TVoid tenv

  and check_cond (c : cond) 
                 (ret : typ)
                 (tenv : typ Env.t) : unit =
    match c with
    | If (e, s) ->
      check e TBool tenv ;
      check_seq s ret tenv
    | If_Else (e, s, c) ->
      check e TBool tenv ;
      check_seq s ret tenv ;
      check_cond c ret tenv
    | Else s ->
      check_seq s ret tenv

  and check_seq (s : seq) 
                (ret : typ)
                (tenv : typ Env.t) : unit =
    List.iter (fun i -> check_instr i ret tenv) s

  and check_mdef (c : class_def)
                 (m : method_def) : unit =
    (* Create local environment with (in this order) :
       global + this + params (we already checked type) + local var of the method *)
    let tenv = Hashtbl.fold (fun x t acc -> Env.add x t acc) p.globals Env.empty in
    let tenv = Env.add "@This" (TClass c.class_name) tenv in
    let tenv = Hashtbl.fold (fun x t acc -> Env.add x t acc) m.locals tenv in
    let tenv = List.fold_left (fun acc (x, t) -> Env.add x t acc) tenv m.params 
    in
    (* Check if method is well-typed *)
    check_seq m.code m.return tenv

  and check_class (c : class_def) : unit =
    Hashtbl.iter (fun _ -> check_mdef c) c.methods ;
    match c.parent with
    | None -> ()
    | Some c_pt ->
      begin
        match Hashtbl.find_opt p.classes c_pt with
        | Some c -> () (* parent exists so it's okay *)
        | None -> 
          error ("unbound value error: class '" ^ c.class_name ^ "' inherits from an inexistent class named '" ^ c_pt ^ "'.")
      end
  in
  Hashtbl.iter (fun _ -> check_class) p.classes ; (* Check classes definitions *)
  check_seq p.main TVoid tenv (* Check main *)