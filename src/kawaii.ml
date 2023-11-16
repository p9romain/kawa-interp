open Format
open Lexing
open Kawa_lib
open Kawa

let file = Sys.argv.(1)

let report (b,e) =
let l = b.pos_lnum in
let fc = b.pos_cnum - b.pos_bol + 1 in
let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  try
    let prog = Kawaparser.program Kawalexer.token lb in
    close_in c;
    



    (* In order to get an interpreter as I started to write, I choose to implment inheritance like this :
       At the start, here, I "flatten" all the inheritance. 

       For exemple, if A has an attribute 'a', and B inherits from A and has an attribute 'b', then in B.attributes
       there are 'a' and 'b', literally.

       If I understood well my OCamL class, it will be memory share so it isn't an issue.(im waiting for something to try
       out if it's true) 
    *)

    (* The type checker has to navigate through inheritance so it's probably not optimal at all as I first thought *)

    (* Get all the methods or attributes we need to inherit *)
    let rec get_all field comp c =
      match c.parent with
      | None -> (field c)
      | Some pt ->
        begin
          match List.find_opt ( fun cl -> cl.class_name = pt ) prog.classes with
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
            (field c) @ (List.filter aux @@ get_all field comp pt_cl)
        end
    in
    (* Change class's attributes and methods (adding inherited ones) *)
    let inherit_obj c = { c with attributes = get_all (fun x -> x.attributes) (fun (e, _) (x, _) -> x = e) c ; 
                                  methods = get_all (fun x -> x.methods) (fun e m-> e.method_name = m.method_name) c } 
    in
    (* Apply changes on every classes *)
    let prog = { prog with classes = List.map inherit_obj prog.classes }





    in
    Typechecker.typecheck_prog prog ;
    Interpreter.exec_prog prog ;
    exit 0
  with
    | Kawalexer.Error s ->
      report (lexeme_start_p lb, lexeme_end_p lb) ;
      eprintf "lexical error: %s@." s ;
      exit 1
    | Kawaparser.Error ->
      report (lexeme_start_p lb, lexeme_end_p lb) ;
      eprintf "syntax error@." ;
      exit 1
    | Interpreter.Error s ->
      eprintf "interpreter error: %s@." s ;
      exit 1
    | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e) ;
      exit 2