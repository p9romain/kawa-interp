open Format
open Lexing
open Kawa_lib

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
    Typechecker.typecheck_prog prog;
    Interpreter.exec_prog prog;
    exit 0
  with
  | Kawalexer.Error s ->
     report (lexeme_start_p lb, lexeme_end_p lb);
     eprintf "lexical error: %s@." s;
     exit 1
  | Kawaparser.Error ->
     report (lexeme_start_p lb, lexeme_end_p lb);
     eprintf "syntax error@.";
     exit 1
  | Interpreter.Error s ->
     eprintf "interpreter error: %s@." s;
     exit 1
  | e ->
     eprintf "Anomaly: %s\n@." (Printexc.to_string e);
     exit 2
