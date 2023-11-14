{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  let () = List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ 
      "true",      BOOL(true) ;
      "false",     BOOL(false) ;

      "null",      NULL ;

      "int",       TYPE(TInt) ;
      (* "float",     TYPE(TFloat) ; *)
      "bool",      TYPE(TBool) ;
      "void",      TYPE(TVoid) ;

      "var",       VAR ;

      "if",        IF ;
      "else",      ELSE ;

      "do",        DO ;
      "while",     WHILE ;
      "for",       FOR ;

      "new",       NEW ;
      "class",     CLASS ;
      "extends",   EXTENDS ;

      "this",      THIS ;
      "attribute", ATTR ;

      "method",    METHOD ;
      "return",    RETURN ;

      "main",      MAIN ;
      "print",     PRINT
    ]
  in
  fun s ->
    match Hashtbl.find_opt h s with
    | Some k -> k
    | None -> IDENT(s)
    
  let float_of_string f = 0.0 (* TODO *)
}

let digit = ['0'-'9']
let decimals = '.' digit+

let integers = ( ['1'-'9'] digit+ ) | digit 

let exponent = ['e' 'E'] '-'? integers
let floats = decimals | ( integers decimals ) | ( integers decimals? exponent )

let ident = ['a'-'z' '_'] (['a'-'z' 'A'-'Z'] | '_' | digit)*
  
rule token = parse
  | ['\n']           { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+ { token lexbuf }

  | "//" [^ '\n']* "\n" { new_line lexbuf; token lexbuf }
  | "/*"                { comment lexbuf; token lexbuf }

  | integers as n { INT(int_of_string n) }
  | floats as f { FLOAT(float_of_string f) }
  | ident as id { keyword_or_ident id }

  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | ";"  { SEMI }

  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | "/"  { SLASH }
  | "%"  { MOD }

  | "!"  { NOT }
  | "&&" { AND }
  | "||" { OR  }

  | "<=" { LE }
  | "<"  { LT }
  | ">=" { GE }
  | ">"  { GT }
  | "==" { EQ }
  | "!=" { NEQ }

  | "="  { SET }

  | "?"  { INTERO }
  | ":"  { TWO_PT }
  
  | "."  { DOT }
  | ","  { COMMA }

  | _   { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
