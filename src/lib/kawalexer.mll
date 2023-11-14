{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  let () = List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ 
      "print", PRINT ;
      "main",  MAIN ;

      "true",  BOOL(true) ;
      "false", BOOL(false) ;

      "var",   VAR ;
      "int",   TYPE(TInt) ;
      "bool",  TYPE(TBool) ;
      "void",  TYPE(TVoid) ;

      "if",    IF ;
      "else",  ELSE ;

      "while", WHILE ;
      "for",   FOR
    ]
  in
  fun s ->
    match Hashtbl.find_opt h s with
    | Some k -> k
    | None -> IDENT(s)
    
}

let digit = ['0'-'9']
let decimals = '.' digit+

let integers = ( ['1'-'9'] digit+ ) | digit 

let exponent = ['e' 'E'] '-'? integers
let floats = decimals | ( integers decimals ) | ( integers decimals? exponent )

let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
let type_str = "int" | "bool" | "void" | ident
  
rule token = parse
  | ['\n']           { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+ { token lexbuf }

  | "//" [^ '\n']* "\n" { new_line lexbuf; token lexbuf }
  | "/*"                { comment lexbuf; token lexbuf }

  | integers as n { INT(int_of_string n) }
  | ident as id { keyword_or_ident id }

  | "=" { SET }

  | "&&" { AND }
  | "||" { OR  }
  | "!"  { NOT }

  | "<=" { LE }
  | "<"  { LT }
  | ">=" { GE }
  | ">"  { GT }
  | "==" { EQ }
  | "!=" { NEQ }

  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { SLASH }
  | "%" { MOD }

  | "?" { INTERO }
  | ":" { TWO_PT }

  | ";" { SEMI }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { BEGIN }
  | "}" { END }

  | _   { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
