open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | Null
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

let exec_prog p =
  let global_env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add global_env x Null) p.globals;

  (* Get all the methods or attributes we need to inherit *)
  let rec get_all field comp c =
    match c.parent with
    | None -> (field c)
    | Some pt ->
      begin
        match List.find_opt ( fun cl -> cl.class_name = pt ) p.classes with
        | None -> failwith ("unbound value error: '" ^ pt ^ "' class is not declared in the scope.")
        | Some pt_cl ->
          (* If the child has a method/an attribute called 'A', and inherit from a class who also has 
            a method/an attribute called 'A', then we keep the child's one.
          *)
            let aux e = (* get rid of duplicates *)
              match List.find_opt (comp e) (field c) with
              | None -> true
              | Some _ -> false
            in
            List.append (field c) (List.filter aux (get_all field comp pt_cl))
      end
  in
  (* Change class's attributes and methods (adding inherited ones) *)
  let inherit c = { c with attributes = get_all (fun x -> x.attributes) (fun (e, _) (x, _) -> x = e) c ; 
                     methods = get_all (fun x -> x.methods) (fun e m-> m.method_name = e.method_name) c } 
  in
  (* Apply changes on every classes *)
  let p = { p with classes = List.map inherit p.classes }
  in

  let rec exec_meth f this args =
    (* Use an upper letter because i'm the only one who is allowed to do it (privelege) *)
    Hashtbl.add args "This" (VObj this) ;
    Hashtbl.add args "Return" Null ;
    (* Add local variables *)
    List.iter (fun (x, _) -> Hashtbl.add args x Null) f.locals ;
    (* Execute method *)
    exec_seq f.code args ;
    (* Get the result *)
    let res = Hashtbl.find_opt args "Return" in
    (* Destroy the local environment *)
    Hashtbl.clear args ;
    (* Return the result *)
    match res with
    | Some v -> v (* If unchanged (so void method), return Null *)
    | None -> Null (* Impossible because always at Null at start (but security) *)

  and exec_seq s local_env =
    let rec eval_unop op e =
      match op with
      | Opp ->
        begin
          match eval e with
          | VInt n -> VInt (-n)
          | _ -> failwith "type error: opposite operator can only be use on integers"
        end
      | Not ->
        begin
          match eval e with
          | VBool b -> VBool (not b)
          | _ -> failwith "type error: negative operator can only be use on booleans"
        end

    and eval_binop op e1 e2 =
      (* Manage operators with two numbers, and return a number *)
      let num_to_num op =
         match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt (op n1 n2)
          | _ -> failwith "type error: integer binary operator can only be use on integers"
      (* Manage operators with two booleans, and return a boolean *)
      in let bool_to_bool op =
        match eval e1, eval e2 with
          | VBool b1, VBool b2 -> VBool (op b1 b2)
          | _ -> failwith "type error: boolean binary operator can only be use on booleans"
      (* Manage operators with two numbers, and return a boolean *)
      in let num_to_bool op =
        match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool (op n1 n2)
          | _ -> failwith "type error: integer binary operator can only be use on integers"
      in
      match op with
      | Add -> num_to_num ( + )
      | Sub -> num_to_num ( - )
      | Mul -> num_to_num ( * )
      | Div -> num_to_num ( / )
      | Mod -> num_to_num ( mod )
      | Le -> num_to_bool ( <= )
      | Lt -> num_to_bool ( < )
      | Ge -> num_to_bool ( >= )
      | Gt -> num_to_bool ( > )
      | Eq ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool (n1 = n2)
          | VBool b1, VBool b2 -> VBool (b1 = b2)
          | _ -> failwith "type error: comparison can only be use on integers and booleans"
        end
      | Neq ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool (n1 <> n2)
          | VBool b1, VBool b2 -> VBool (b1 <> b2)
          | _ -> failwith "type error: comparison can only be use on integers and booleans"
        end
      | And -> bool_to_bool ( && )
      | Or -> bool_to_bool ( || )

    and eval_call o m_name arg =
      (* Check if the class exists *)
      match List.find_opt (fun cl -> cl.class_name = o.cls ) p.classes with
        | None -> failwith ("unbound value error: '" ^ o.cls ^ "' class is not declared in the scope.")
        | Some c ->
          begin
            (* Check if the method in the class exists *)
            match List.find_opt (fun m -> m.method_name = m_name ) c.methods with
              | None -> 
                if m_name = "constructor" then
                  failwith ("unbound value error: can't find the constructor method to initialize the object")
                else
                  failwith ("unbound value error: '" ^ m_name ^ "' method is not declared in the class '" ^ o.cls ^ "'.")
              | Some m ->
                let args = Hashtbl.create 5 in
                (* Add all the parameters in the local environment *)
                List.iter2 (fun e (x, _) -> Hashtbl.add args x (eval e) ) arg m.params ;
                (* Start the method call *)
                exec_meth m o args
          end

    and eval e =
      match e with
      | Int n  -> VInt n
      | Bool b -> VBool b
      | Unop (op, e) -> eval_unop op e
      | Binop (op, e1, e2) -> eval_binop op e1 e2
      | TerCond(e1, e2, e3) ->
        begin
          match eval e1 with
          | VBool b ->
            if b then
              eval e2
            else
              eval e3 
          | _ -> failwith "type error: can't evaluate anything else than a boolean as a condition test" 
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
                  | None -> failwith ("unbound value error: '" ^ s ^ "' is not declared in the scope.")
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
                  | None -> failwith ("unbound value error: can't acces the field '" ^ s ^ "' in the object of type '" ^ o.cls ^ "'.")
                end
              | _ -> failwith "type error: can't access to a field of a non-object"
            end
        end
      | This ->
        begin 
          (* Check if the variable is in the local environment *)
          match Hashtbl.find_opt local_env "This" with
          | Some v -> v
          | None -> failwith ("unbound value error: 'this' is not declared in the scope.\nMaybe you are outside a class ?")
        end
      | New s ->
        (* Create a new object *)
        let o = { cls = s ; fields = Hashtbl.create 5 } in
        begin
          (* Check if the class exists *)
          match List.find_opt (fun cl -> cl.class_name = s ) p.classes with
          | None -> failwith ("unbound value error: '" ^ s ^ "' class is not declared in the scope.")
          | Some c ->
            (* Set up all the attributes to Null *)
            List.iter (fun (x, _) -> Hashtbl.add o.fields x Null ) c.attributes ;
            VObj o
        end
      | NewCstr (s, el) ->
        (* Create a new object *)
        let VObj(o) = eval (New s) in
        (* Call the constructor *)
        let _ = eval_call o "constructor" el in
        VObj o
      | MethCall (e, s, el) ->
        begin
          (* Check if it's an object *)
          match eval e with
          | VObj o -> eval_call o s el
          | _ -> failwith "type error: can't access to a field of a non-object"
        end
    in
    let rec exec i = 
      match i with
      | Print e -> 
        begin
          match eval e with
          | VInt n -> Printf.printf "%d\n" n
          | VBool b -> Printf.printf "%s\n" (if b then "true" else "false")
          | _ -> failwith "type error: can't print other type than int or bool." 
        end
      | Set (m, e) ->
        begin
          match m with
          | Var s -> 
            begin
              (* Check if the variable is in the local environment *)
              match Hashtbl.find_opt local_env s with
              | Some _ -> Hashtbl.add local_env s (eval e)
              | None ->
                begin
                  (* Check if the variable is in the global environment *)
                  match Hashtbl.find_opt global_env s with
                  | Some _ -> Hashtbl.add global_env s (eval e)
                  | None -> failwith ("unbound value error: '" ^ s ^ "' is not declared in the scope.")
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
                  | Some _ -> Hashtbl.add o.fields s (eval e)
                  | None -> failwith ("unbound value error: can't acces the field '" ^ s ^ "' in the object of type '" ^ o.cls ^ "'.")
                end
              | _ -> failwith "type error: can't access to a field of a non-object"
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
          | _ -> failwith "type error: can't evaluate anything else than a boolean as a condition test"
        end
      | DoWhile (s, w) ->
        exec_seq s local_env ; (* do *)
        exec_seq [ w ] local_env (* while *)
      | Cond c -> exec_cond c
      | Return e -> Hashtbl.add local_env "Return" (eval e)
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
          | _ -> failwith "type error: can't evaluate anything else than a boolean as a condition test"
        end
      | If_Else (e, s, c) ->
        begin
          match eval e with
          | VBool b ->
            if b then
              exec_seq s local_env
            else
              exec_cond c
          | _ -> failwith "type error: can't evaluate anything else than a boolean as a condition test"
        end
      | Else s ->
        exec_seq s local_env

    in
    List.iter exec s
  in
  exec_seq p.main (Hashtbl.create 1)