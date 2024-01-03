open Format
open Lexing
open Kawa_lib
open Kawa

let file = Sys.argv.(1)
let file_name = List.hd @@ List.rev @@ String.split_on_char '/' file 

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
    (* Typechecker.typecheck_prog prog ; *)
    Interpreter.exec_prog prog ;
    exit 0
  with
    | Kawalexer.Error s ->
      report (lexeme_start_p lb, lexeme_end_p lb) ;
      eprintf "in file '%s' :\nlexical error: %s@." file_name s ;
      exit 1
    | Kawaparser.Error ->
      report (lexeme_start_p lb, lexeme_end_p lb) ;
      eprintf "in file '%s' :\nsyntax error@." file_name ;
      exit 1
    | Interpreter.Error s ->
      eprintf "in file '%s' :\ninterpreter error: %s@." file_name s ;
      exit 1
    | e ->
      eprintf "in file '%s' :\nAnomaly: %s@." file_name (Printexc.to_string e) ;
      exit 2