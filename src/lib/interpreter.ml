open Kawa

type value =
  | VInt  of int
  | VFloat of float
  | VString of string
  | VBool of bool
  | VObj  of obj
  | VNull
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

(* For printing an object and any value *)
let rec string_of_obj o =
  Printf.sprintf "Object<%s>(%s)" o.cls (Hashtbl.fold 
    (fun k v acc -> 
      acc ^ (if acc = "" then "" else "; ") 
          ^ (Printf.sprintf "%s = %s" k (string_of_value v)) 
    ) 
  o.fields "")
and string_of_value v =
  match v with
  | VInt n -> string_of_int n
  | VFloat f -> Printf.sprintf "%F" f
  | VString s -> s
  | VBool b -> if b then "true" else "false"
  | VObj o' -> string_of_obj o'
  | VNull -> "null"

let typ_of_value v =
  match v with
  | VInt _ -> TInt
  | VFloat _ -> TFloat
  | VBool _ -> TBool
  | VString _ -> TString
  | VObj o -> TClass (o.cls)
  | VNull -> TVoid

(* To manage overloading : method names are "Name@@[types]" *)
let method_name_type m_name typ_list = 
  m_name  ^ "@@[" 
          ^ (List.fold_left 
              (fun acc t -> acc
                          ^ (if acc = "" then "" else "; ") 
                          ^ (Printf.sprintf "%s" (typ_to_string t))  
              )  
            "" typ_list) 
          ^ "]"
let method_name_expr eval m_name expr_list =
  m_name  ^ "@@[" 
          ^ (List.fold_left 
              (fun acc e -> acc
                          ^ (if acc = "" then "" else "; ") 
                          ^ (Printf.sprintf "%s" (typ_to_string @@ typ_of_value @@ eval e))  
              ) 
            "" expr_list) 
          ^ "]"

(* To get classes of name [c_name]*)
let get_class cls_hash c_name =
  (* Check if the class exists *)
  match Hashtbl.find_opt cls_hash c_name with
  | Some cl -> cl
  | None -> raise (Error ("unbound value error: '" ^ c_name ^ "' class is not declared in the program."))

(* Get the value of [m] in the [local_env] and then in the [global_env]

  We also give [eval] because it is out of its definition
  We also give a function [f] to use mem_access in different usage (get the value vs 
      assignment which is a quite similar code !)
 *)
let mem_access global_env local_env m eval f =
  match m with
  | Var s ->
    begin 
      (* Check if the variable is in the local environment *)
      match Hashtbl.find_opt local_env s with
      | Some v -> f s local_env v
      | None ->
        begin 
          (* Check if the variable is in the global environment *)
          match Hashtbl.find_opt global_env s with
          | Some v -> f s global_env v
          | None -> raise (Error ("unbound value error: '" ^ s ^ "' is not declared in the scope."))
        end
    end
  | Field (e, s) ->
    begin
      (* Check if it's an object *)
      match eval e with
      | VObj o -> 
        begin
          (* Check if the field exists *)
          match Hashtbl.find_opt o.fields s with
          | Some v -> f s o.fields v
          | None -> raise (Error ("unbound value error: can't access the field '" ^ s 
                                     ^ "' in the object of class '" ^ o.cls ^ "'."))
        end
      | _ -> failwith "Impossible : typechecker's work"
    end

(* Execute the main of [p] *)
let exec_prog p =
  let global_env = Hashtbl.create 16 in
  (* alias *)
  let get_class = get_class p.classes in 
  Hashtbl.iter (fun x _ -> Hashtbl.replace global_env x VNull) p.globals;

  (* Execute the method [f args] in the object [this] *)
  let rec exec_meth f this args =
    (* Use an @ because i'm the only one who is allowed to do it (privelege) *)
    Hashtbl.replace args "@This" (VObj this) ;
    (* Add local variables *)
    Hashtbl.iter (fun x _ -> Hashtbl.replace args x VNull) f.locals ;
    (* Execute method *)
    try
      exec_seq f.code args ;
      VNull
    (* Get the result if there is a return *)
    with Return v ->
      match v, f.return with
      | VInt n, TFloat -> VFloat (float n)
      | _ -> v

  (* Execute a seq [s] with the local environment [local_env] *)
  and exec_seq s local_env =
    (* alias *)
    let mem_access = mem_access global_env local_env in
    (* Evaluate an expression [e] with the unary operator [op] *)
    let rec eval_unop op e =
      match op with
      | Opp ->
        begin
          match eval e with
          | VInt n -> VInt (-n)
          | VFloat f -> VFloat (-.f)
          | _ -> failwith "Impossible : typechecker's work"
        end
      | Not ->
        begin
          match eval e with
          | VBool b -> VBool (not b)
          | _ -> failwith "Impossible : typechecker's work"
        end

    (* Evaluate two expressions [e1] and [e2] with the binary operator [op] *)
    and eval_binop op e1 e2 =
      let bool_to_bool op =
        match eval e1, eval e2 with
        | VBool b1, VBool b2 -> VBool (op b1 b2)
        | _ -> failwith "Impossible : typechecker's work"
      in
      (* Manage the int and float conversion *)
      let num_to_num op_int op_float =
        match eval e1, eval e2 with
        | VInt n1, VInt n2 -> VInt (op_int n1 n2)
        | VFloat f, VInt n -> VFloat (op_float f (float n))
        | VInt n, VFloat f -> VFloat (op_float (float n) f)
        | VFloat f1, VFloat f2 -> VFloat (op_float f1 f2)
        | _ -> failwith "Impossible : typechecker's work"
      in
      (* Manage the int and float conversion 

        Because of type inference there is two operator even 
          it's the same one for both..... 
      *)
      let compare op_int op_float =
        match eval e1, eval e2 with
        | VInt n1, VInt n2 -> VBool (op_int n1 n2)
        | VFloat f, VInt n -> VBool (op_float f (float n))
        | VInt n, VFloat f -> VBool (op_float (float n) f)
        | VFloat f1, VFloat f2 -> VBool (op_float f1 f2)
        | _ -> failwith "Impossible : typechecker's work"
      in
      match op with
      | Add ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt (n1 + n2)
          | VFloat f, VInt n -> VFloat (f +. (float n))
          | VInt n, VFloat f -> VFloat ((float n) +. f)
          | VFloat f1, VFloat f2 -> VFloat (f1 +. f2)
          | VString s1, VString s2 -> VString(s1 ^ s2)
          | _ -> failwith "Impossible : typechecker's work"
        end
      | Sub -> num_to_num (-) (-.)
      | Mul -> num_to_num ( * ) ( *. )
      | Div -> num_to_num (/) (/.)
      | Mod ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt (n1 mod n2)
          | _ -> failwith "Impossible : typechecker's work"
        end
      | Le -> compare (<=) (<=)
      | Lt -> compare (<) (<)
      | Ge -> compare (>=) (>=)
      | Gt -> compare (>) (>)
      | Eq ->
        begin
          match eval e1, eval e2 with
          | VBool x, VBool y -> VBool (x = y)
          (* Two objects are equal if and only if they are physically the same object 
             And we have "(==) => (=)", so we have '&&' *)
          | VObj o1, VObj o2 -> VBool (o1 == o2 && o1 = o2)
          | VNull, VNull -> VBool(true)
          | VString s, v -> VBool(s = string_of_value v)
          | v, VString s -> VBool(string_of_value v = s)
          (* For integers and floats *)
          | _ -> compare (=) (=)
        end
      | Neq ->
        begin
          match eval e1, eval e2 with
          | VBool x, VBool y -> VBool (x <> y)
          (* Two objects are equal if and only if they are physically the same object 
             And we have "(==) => (=)", so we have '&&' *)
          | VObj o1, VObj o2 -> VBool (o1 != o2 && o1 <> o2)
          | VNull, VNull -> VBool(false)
          | VString s, v -> VBool(s <> string_of_value v)
          | v, VString s -> VBool(string_of_value v <> s)
          (* For integers and floats *)
          | _ -> compare (<>) (<>)
        end
      | And -> bool_to_bool (&&)
      | Or ->  bool_to_bool (||)

    (* Evaluate the call of the [o]'s method [m_name arg]*)
    and eval_call o m_name arg =
      (* Call the method [m]*)
      let method_call m =
        let args = Hashtbl.create 5 in
        (* For readability *)
        let assignment t e var_name =
          match t, eval e with
          | TFloat, VInt n -> Hashtbl.replace args var_name (VFloat (float n))
          | _ -> Hashtbl.replace args var_name (eval e)
        in
        (* Add all the parameters in the local environment *)
        List.iter2 (fun e (x, t) -> assignment t e x ) arg m.params ;
        (* Start the method call *)
        exec_meth m o args
      in
      let c = get_class o.cls in
      let rec inheritance c =
        (* Check if the method exists *)
        match Hashtbl.find_opt c.methods m_name with
        | Some m -> method_call m
        | None -> 
          begin
            (* If not, check for the parent *)
            match c.parent with
            | Some p -> inheritance (get_class p)
            | None -> raise (Error ("unbound value error: can't access the method '" ^ m_name ^ "' in the object of class '" ^ c.class_name ^ "'.")) 
          end
      in
      inheritance c

    (* Evaluate an expression [e] *)
    and eval e =
      match e with
      | Int n -> VInt n
      | Float f -> VFloat f
      | String s -> VString s
      | Bool b -> VBool b
      | Null -> VNull
      | Unop (op, e) -> eval_unop op e
      | Binop (op, e1, e2) -> eval_binop op e1 e2
      | TerCond (t, e1, e2) ->
        begin
          match eval t with
          | VBool b ->
            if b then
              eval e1
            else
              eval e2
          | _ -> failwith "Impossible : typechecker's work"
        end
      | InstanceOf (e, t) ->
        begin
          let typ_e = typ_of_value (eval e) in
          match typ_e, t with
          | TInt, TInt
          | TFloat, TFloat
          | TString, TString
          | TBool, TBool -> VBool true
          | TClass c1, TClass c2 ->
            (* Check if the class exists *)
            let _ = get_class c2 in
            let rec check_inheritance_type c =
              if c <> c2 then
                let cl = get_class c in
                  match cl.parent with
                  | Some s_pt -> check_inheritance_type s_pt
                  | None -> VBool false
              else
                VBool true
            in
            check_inheritance_type c1
          | _, _ -> VBool false
        end
      | Get m -> mem_access m eval (fun _ _ x -> x)
      | This ->
        begin 
          (* Check if the variable is in the local environment *)
          match Hashtbl.find_opt local_env "@This" with
          | Some v -> v
          | None -> raise (Error "unbound value error: can't access to 'this'.\nHint : are you inside a class ?")
        end
      | New s ->
        (* Create a new object *)
        let o = { cls = s ; fields = Hashtbl.create 5 } in
        let c = get_class s in
        let () = Hashtbl.iter (fun x _ -> Hashtbl.replace o.fields x VNull ) c.attributes in
        VObj o
      | NewCstr (s, el) ->
        (* Create a new object *)
        let VObj(o) = eval (New s) in
        (* Call the constructor called by the same name as the class, so [s]*)
        let _ = eval_call o (method_name_expr eval s el) el in
        VObj o
      | MethCall (e, s, el) ->
        begin
          match eval e with
          | VObj o -> eval_call o (method_name_expr eval s el) el
          | _ -> failwith "Impossible : typechecker's work"
        end
    
    in
    (* Execute an instruction [i] *)
    let rec exec i = 
      match i with
      | Print e -> Printf.printf "%s\n" (string_of_value (eval e))
      | Assert e ->
        begin
          match eval e with
          | VBool b ->
            if not b then
              raise (Error "AssertionError")

          | _ -> failwith "Impossible : typechecker's work"
        end
      | Set (m, s, e) ->
        begin
          (* For readability *)
          let assignment new_value var_name hash_tab old_value =
            let () = 
              begin
                match old_value, new_value with
                (* type conversion, then assignment *)
                | VFloat _, VInt n -> Hashtbl.replace hash_tab var_name (VFloat (float n))
                (* direct assignment *)
                | _ -> Hashtbl.replace hash_tab var_name new_value
              end
            in
            (* You may ask, why? Because of OCamL type inference and the alias mem_access*)
            VNull
          in
          let op_then_set op =
            let value_to_expr v =
              match v with
              | VInt n -> Int n
              | VFloat f -> Float f
              | VString s -> String s
              (* We are talking about arithmetic so... *)
              | _ -> failwith "Impossible : typechecker's work."
            in
            (* Get the variable *)
            let var = eval (Get(m)) in
            (* Evaluate before assigning *)
            let var_bop = eval (Binop(op, (value_to_expr var), e)) in
            (* Assign *)
            let _ = mem_access m eval (assignment var_bop) in
            ()
          in
          match s with
          | S_Set -> 
            let _ = mem_access m eval (assignment (eval e)) in
            ()
          | S_Add -> op_then_set Add
          | S_Sub -> op_then_set Sub
          | S_Mul -> op_then_set Mul
          | S_Div -> op_then_set Div
        end
      | While (e, s) ->
        begin
          match eval e with
          | VBool b ->
            if b then
              begin
                exec_seq s local_env ;
                exec i
              end
            else
              ()
          | _ -> failwith "Impossible : typechecker's work"
        end
      | DoWhile (s, w) ->
        exec_seq s local_env ; (* do *)
        exec w (* while *)
      | Cond c -> exec_cond c
      | Return e -> raise (Return (eval e))
      | Expr e -> let _ = eval e in ()

    (* Execute a condition instruction [c] *)
    and exec_cond c =
      match c with
      | If (e, s) ->
        begin
          match eval e with
          | VBool b ->
            if b then
              exec_seq s local_env
          | _ -> failwith "Impossible : typechecker's work"
        end
      | If_Else (e, s, c) ->
        begin
          match eval e with
          | VBool b ->
            if b then
              exec_seq s local_env
            else
              exec_cond c
          | _ -> failwith "Impossible : typechecker's work"
        end
      | Else s -> exec_seq s local_env

    in
    (* Execute the sequence [s] *)
    List.iter exec s
  in
  exec_seq p.main (Hashtbl.create 1)