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

exception Error of string
exception Return of value

let exec_prog p =
  let env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  
  let rec eval_call f this args =
    failwith "eval_call not implemented"

  and exec_seq s lenv =
    let rec eval_unop op e = 
      match op with
      | Opp ->
        begin
          match eval e with
          | VInt n -> VInt (-n)
          | _ -> failwith "type error : '-' unary operator can only be use on integers"
        end
      | Not ->
        begin
          match eval e with
          | VBool b -> VBool (not b)
          | _ -> failwith "type error : '!' unary operator can only be use on booleans"
        end
      | _ -> assert false

    and eval_binop op e1 e2 = 
      match op with
      | Add ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt (n1 + n2)
          | _ -> failwith "type error : '+' binary operator can only be use on integers"
        end
      | Sub ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt (n1 - n2)
          | _ -> failwith "type error : '-' binary operator can only be use on integers"
        end
      | Mul ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt (n1 * n2)
          | _ -> failwith "type error : '*' binary operator can only be use on integers"
        end
      | Div ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt (n1 / n2)
          | _ -> failwith "type error : '/' binary operator can only be use on integers"
        end
      | Mod ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VInt (n1 mod n2)
          | _ -> failwith "type error : '%' binary operator can only be use on integers"
        end
      | Le ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool (n1 <= n2)
          | _ -> failwith "type error : '<=' binary operator can only be use on integers"
        end
      | Lt ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool (n1 < n2)
          | _ -> failwith "type error : '<' binary operator can only be use on integers"
        end
      | Ge ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool (n1 >= n2)
          | _ -> failwith "type error : '>=' binary operator can only be use on integers"
        end
      | Gt ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool (n1 > n2)
          | _ -> failwith "type error : '>' binary operator can only be use on integers"
        end
      | Eq ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool (n1 = n2)
          | VBool b1, VBool b2 -> VBool (b1 = b2)
          | _ -> failwith "type error : '==' binary operator can only be use on integers and booleans"
        end
      | Neq ->
        begin
          match eval e1, eval e2 with
          | VInt n1, VInt n2 -> VBool (n1 <> n2)
          | VBool b1, VBool b2 -> VBool (b1 <> b2)
          | _ -> failwith "type error : '!=' binary operator can only be use on integers and booleans"
        end
      | And ->
        begin
          match eval e1, eval e2 with
          | VBool b1, VBool b2 -> VBool (b1 && b2)
          | _ -> failwith "type error : '&&' binary operator can only be use on booleans"
        end
      | Or ->
        begin
          match eval e1, eval e2 with
          | VBool b1, VBool b2 -> VBool (b1 || b2)
          | _ -> failwith "type error : '||' binary operator can only be use on booleans"
        end
      | _ -> assert false

    and eval e =
      match e with
      | Int n  -> VInt n
      | Bool b -> VBool b
      | Binop (op, e1, e2) -> eval_binop op e1 e2
      | Unop (op, e) -> eval_unop op e
      | _ -> failwith "case not implemented in eval"
    in
    let rec exec i = 
      match i with
      | Print e -> 
        begin
          match eval e with
          | VInt n -> Printf.printf "%d\n" n
          | VBool b -> Printf.printf "%s\n" (if b then "true" else "false")
          | _ -> failwith "Print error : can't print other type than int or bool." 
        end
      | _ -> failwith "case not implemented in exec"

    and exec_seq s = 
      List.iter exec s
    in
    exec_seq s
  in
  exec_seq p.main (Hashtbl.create 1)