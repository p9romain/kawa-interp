open Kawa

type value =
  | VInt  of int
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
          | _ -> failwith "Impossible : type_check work"
        end
      | Not ->
        begin
          match eval e with
          | VBool b -> VBool (not b)
          | _ -> failwith "Impossible : type_check work"
        end

    and eval_binop op e1 e2 =
      match op with
      | Add ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt ((+) n1 n2)
          | _ -> failwith "Impossible : type_check work"
        end
      | Sub ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt ((-) n1 n2)
          | _ -> failwith "Impossible : type_check work"
        end
      | Mul ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt (( * ) n1 n2)
          | _ -> failwith "Impossible : type_check work"
        end
      | Div ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt ((/) n1 n2)
          | _ -> failwith "Impossible : type_check work"
        end
      | Mod ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt ((mod) n1 n2)
          | _ -> failwith "Impossible : type_check work"
        end
      | Le ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool ((<=) n1 n2)
          | _ -> failwith "Impossible : type_check work"
        end
      | Lt ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool ((<) n1 n2)
          | _ -> failwith "Impossible : type_check work"
        end
      | Ge ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool ((>=) n1 n2)
          | _ -> failwith "Impossible : type_check work"
        end
      | Gt ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool ((>) n1 n2)
          | _ -> failwith "Impossible : type_check work"
        end
      | Eq ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool (n1 = n2)
          | VBool b1, VBool b2 -> VBool (b1 = b2)
          (* Two objects are equal if and only if they are physically the same object 
             And we have "(==) => (=)", so we have '&&' *)
          | VObj o1, VObj o2 -> VBool (o1 == o2 && o1 = o2)
          | VNull, VNull -> VBool(true)
          | _ -> failwith "Impossible : type_check work"
        end
      | Neq ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool (n1 <> n2)
          | VBool b1, VBool b2 -> VBool (b1 <> b2)
          (* Two objects are equal if and only if they are physically the same object 
             And we have "(==) => (=)", so we have '&&' *)
          | VObj o1, VObj o2 -> VBool (o1 != o2 && o1 <> o2)
          | VNull, VNull -> VBool(false)
          | _ -> failwith "Impossible : type_check work"
        end
      | And ->
        begin
          match eval e1, eval e2 with
          | VBool b1, VBool b2 -> VBool ((&&) b1 b2)
          | _ -> failwith "Impossible : type_check work"
        end
      | Or ->
        begin
          match eval e1, eval e2 with
          | VBool b1, VBool b2 -> VBool ((||) b1 b2)
          | _ -> failwith "Impossible : type_check work"
        end

    and eval_call o m_name arg =
      (* Check if the class exists *)
      match List.find_opt (fun cl -> cl.class_name = o.cls ) p.classes with
        | None -> failwith "Impossible : type_check work"
        | Some c ->
          begin
            (* Check if the method in the class exists *)
            match List.find_opt (fun m -> m.method_name = m_name ) c.methods with
              | None -> 
                if m_name = "constructor" then
                  failwith "Impossible : type_check work"
                else
                  failwith "Impossible : type_check work"
              | Some m ->
                let args = Hashtbl.create 5 in
                (* Add all the parameters in the local environment *)
                List.iter2 (fun e (x, _) -> Hashtbl.replace args x (eval e) ) arg m.params ;
                (* Start the method call *)
                exec_meth m o args
          end

    and eval e =
      match e with
      | Int n -> VInt n
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
          | _ -> failwith "Impossible : type_check work"
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
                  | None -> failwith "Impossible : type_check work"
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
                  | None -> failwith "Impossible : type_check work"
                end
              | _ -> failwith "Impossible : type_check work"
            end
        end
      | This ->
        begin 
          (* Check if the variable is in the local environment *)
          match Hashtbl.find_opt local_env "@This" with
          | Some v -> v
          | None -> failwith "Impossible : type_check work"
        end
      | New s ->
        (* Create a new object *)
        let o = { cls = s ; fields = Hashtbl.create 5 } in
        begin
          (* Check if the class exists *)
          match List.find_opt (fun cl -> cl.class_name = s ) p.classes with
          | None -> failwith "Impossible : type_check work"
          | Some c ->
            (* Set up all the attributes to VNull *)
            List.iter (fun (x, _) -> Hashtbl.replace o.fields x VNull ) c.attributes ;
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
          | _ -> failwith "Impossible : type_check work"
        end
    in
    let rec exec i = 
      match i with
      | Print e -> 
        begin
          match eval e with
          | VInt n -> Printf.printf "%d\n" n
          | VBool b -> Printf.printf "%s\n" (if b then "true" else "false")
          | VObj o -> Printf.printf "%s\n" (string_of_obj o) 
          | VNull -> Printf.printf "null\n"
        end
      | Set (m, e) ->
        begin
          match m with
          | Var s -> 
            begin
              (* Check if the variable is in the local environment *)
              match Hashtbl.find_opt local_env s with
              | Some _ -> Hashtbl.replace local_env s (eval e)
              | None ->
                begin
                  (* Check if the variable is in the global environment *)
                  match Hashtbl.find_opt global_env s with
                  | Some _ -> Hashtbl.replace global_env s (eval e)
                  | None -> failwith "Impossible : type_check work"
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
                  | Some _ -> Hashtbl.replace o.fields s (eval e)
                  | None -> failwith "Impossible : type_check work"
                end
              | _ -> failwith "Impossible : type_check work"
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
          | _ -> failwith "Impossible : type_check work"
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
          | _ -> failwith "Impossible : type_check work"
        end
      | If_Else (e, s, c) ->
        begin
          match eval e with
          | VBool b ->
            if b then
              exec_seq s local_env
            else
              exec_cond c
          | _ -> failwith "Impossible : type_check work"
        end
      | Else s ->
        exec_seq s local_env

    in
    List.iter exec s
  in
  exec_seq p.main (Hashtbl.create 1)