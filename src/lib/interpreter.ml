open Kawa

type value =
  | VInt  of int
  | VFloat of float
  | VBool of bool
  | VObj  of obj
  | VNull
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

let rec string_of_obj o =
  Printf.sprintf "Object<%s>(%s)" o.cls (Hashtbl.fold 
    (fun k v acc -> 
      acc ^ (if acc = "" then "" else "; ") ^ (Printf.sprintf "%s = %s" k 
        (
          match v with
          | VInt n -> string_of_int n
          | VFloat f -> Printf.sprintf "%F" f
          | VBool b -> if b then "true" else "false"
          | VObj o' -> string_of_obj o'
          | VNull -> "null"
        )
      ) 
    ) 
  o.fields "")

let exec_prog p =
  let global_env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.replace global_env x VNull) p.globals;
  
  let rec exec_meth f this args =
    (* Use an @ because i'm the only one who is allowed to do it (privelege) *)
    Hashtbl.replace args "@This" (VObj this) ;
    (* Add local variables *)
    List.iter (fun (x, _) -> Hashtbl.replace args x VNull) f.locals ;
    (* Execute method *)
    try
      exec_seq f.code args ;
      VNull
    (* Get the result if there is a return *)
    with Return e ->
      e

  and exec_seq s local_env =
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

    and eval_binop op e1 e2 =
      let num_to_num op_int op_float =
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt (op_int n1 n2)
          | VFloat f, VInt n -> VFloat (op_float f (float n))
          | VInt n, VFloat f -> VFloat (op_float (float n) f)
          | VFloat f1, VFloat f2 -> VFloat (op_float f1 f2)
          | _ -> failwith "Impossible : typechecker's work"
        end
      in
      (* because of type inference..... *)
      let compare op_int op_float =
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool (op_int n1 n2)
          | VFloat f, VInt n -> VBool (op_float f (float n))
          | VInt n, VFloat f -> VBool (op_float (float n) f)
          | VFloat f1, VFloat f2 -> VBool (op_float f1 f2)
          | _ -> failwith "Impossible : typechecker's work"
        end
      in
      match op with
      | Add -> num_to_num (+) (+.)
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
          | _ -> compare (=) (=) (* For integers and floats *)
        end
      | Neq ->
        begin
          match eval e1, eval e2 with
          | VBool x, VBool y -> VBool (x <> y)
          (* Two objects are equal if and only if they are physically the same object 
             And we have "(==) => (=)", so we have '&&' *)
          | VObj o1, VObj o2 -> VBool (o1 != o2 && o1 <> o2)
          | VNull, VNull -> VBool(false)
          | _ -> compare (<>) (<>) (* For integers and floats *)
        end
      | And ->
        begin
          match eval e1, eval e2 with
          | VBool b1, VBool b2 -> VBool ((&&) b1 b2)
          | _ -> failwith "Impossible : typechecker's work"
        end
      | Or ->
        begin
          match eval e1, eval e2 with
          | VBool b1, VBool b2 -> VBool ((||) b1 b2)
          | _ -> failwith "Impossible : typechecker's work"
        end

    and eval_call o m_name arg =
      (* Check if the class exists *)
      match List.find_opt (fun cl -> cl.class_name = o.cls ) p.classes with
      | Some c ->
        begin
          (* Check if the method in the class exists *)
          match List.find_opt (fun m -> m.method_name = m_name ) c.methods with
          | Some m ->
            let args = Hashtbl.create 5 in
            let assignment t e s =
              match t, eval e with
              | TInt, VFloat f -> Hashtbl.replace args s (VInt (int_of_float f))
              | TFloat, VInt n -> Hashtbl.replace args s (VFloat (float n))
              | _ -> Hashtbl.replace args s (eval e)
            in
            (* Add all the parameters in the local environment *)
            List.iter2 (fun e (x, t) -> assignment t e x ) arg m.params ;
            (* Start the method call *)
            exec_meth m o args
          | None -> raise (Error "")
        end
      | None -> raise (Error "")

    and eval e =
      match e with
      | Int n -> VInt n
      | Float f -> VFloat f
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
      | Get m ->
        begin
          match m with
          | Var s ->
            begin 
              (* Check if the variable is in the local environment *)
              match Hashtbl.find_opt local_env s with
              | Some v -> v
              | None ->
                begin 
                  (* Check if the variable is in the global environment *)
                  match Hashtbl.find_opt global_env s with
                  | Some v -> v
                  | None -> raise (Error "")
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
                  | Some v -> v
                  | None -> raise (Error "")
                end
              | _ -> failwith "Impossible : typechecker's work"
            end
        end
      | This ->
        begin 
          (* Check if the variable is in the local environment *)
          match Hashtbl.find_opt local_env "@This" with
          | Some v -> v
          | None -> raise (Error "")
        end
      | New s ->
        (* Create a new object *)
        let o = { cls = s ; fields = Hashtbl.create 5 } in
        begin
          (* Check if the class exists *)
          match List.find_opt (fun cl -> cl.class_name = s ) p.classes with
          | Some c ->
            (* Set up all the attributes to VNull *)
            List.iter (fun (x, _) -> Hashtbl.replace o.fields x VNull ) c.attributes ;
            VObj o
          | None -> raise (Error "")
        end
      | NewCstr (s, el) ->
        (* Create a new object *)
        let VObj(o) = eval (New s) in
        (* Call the constructor *)
        let _ = eval_call o "constructor" el in
        VObj o
      | MethCall (e, s, el) ->
        begin
          match eval e with
          | VObj o -> eval_call o s el
          | _ -> failwith "Impossible : typechecker's work"
        end
    in
    let rec exec i = 
      match i with
      | Print e -> 
        begin
          match eval e with
          | VInt n -> Printf.printf "%d\n" n
          | VFloat f -> Printf.printf "%F\n" f 
          | VBool b -> Printf.printf "%s\n" (if b then "true" else "false")
          | VObj o -> Printf.printf "%s\n" (string_of_obj o) 
          | VNull -> Printf.printf "null\n"
        end
      | Assert e ->
        begin
          match eval e with
          | VBool b ->
            if not b then
              raise (Error "AssertionError")
          | _ -> failwith "Impossible : typechecker's work"
        end
      | Set (m, e) ->
        begin
          let assignment v s hash_tab =
            match v, eval e with
            | VInt _, VFloat f -> Hashtbl.replace hash_tab s (VInt (int_of_float f))
            | VFloat _, VInt n -> Hashtbl.replace hash_tab s (VFloat (float n))
            | _ -> Hashtbl.replace hash_tab s (eval e)
          in
          match m with
          | Var s -> 
            begin
              (* Check if the variable is in the local environment *)
              match Hashtbl.find_opt local_env s with
              | Some v -> assignment v s local_env
              | None ->
                begin
                  (* Check if the variable is in the global environment *)
                  match Hashtbl.find_opt global_env s with
                  | Some v -> assignment v s global_env
                  | None -> raise (Error "")
                end
            end
          | Field (e', s) ->
            begin
              (* Check if it's an object *)
              match eval e' with
              | VObj o -> 
                begin
                  (* Check if the field exists *)
                  match Hashtbl.find_opt o.fields s with
                  | Some v -> assignment v s o.fields
                  | None -> raise (Error "")
                end
              | _ -> failwith "Impossible : typechecker's work"
            end
        end
      | While (e, s) ->
        begin
          match eval e with
          | VBool b ->
            if b then
              begin
                exec_seq s local_env ;
                exec_seq [ While(e, s) ] local_env
              end
            else
              ()
          | _ -> failwith "Impossible : typechecker's work"
        end
      | DoWhile (s, w) ->
        exec_seq s local_env ; (* do *)
        exec_seq [ w ] local_env (* while *)
      | Cond c -> exec_cond c
      | Return e -> raise (Return (eval e))
      | Expr e -> 
        match eval e with
        | _ -> ()

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
      | Else s ->
        exec_seq s local_env

    in
    List.iter exec s
  in
  exec_seq p.main (Hashtbl.create 1)