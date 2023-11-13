%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT

%token LPAR RPAR BEGIN END SEMI

%token U_MINUS
%token PLUS MINUS TIMES SLASH MOD

%token NOT AND OR
%token LE LT GE GT EQ NEQ

%token MAIN
%token PRINT
%token EOF



%left OR
%left AND
%left EQ NEQ
%left LE LT GE GT
%nonassoc NOT

%left PLUS MINUS 
%left TIMES SLASH MOD
%nonassoc U_MINUS



%start program
%type <Kawa.program> program

%%

program:
| MAIN BEGIN main=list(instruction) END EOF
    { {classes=[]; globals=[]; main} }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
;

expression:
| n=INT                                   { Int(n)            }
| b=BOOL                                  { Bool(b)           }
| LPAR e=expression RPAR                  { e                 }
| e1=expression op=bop e2=expression      { Binop(op, e1, e2) }
| op=uop e=expression                     { Unop(op, e)       }
;


%inline bop :
| PLUS  { Add }
| MINUS { Sub }
| TIMES { Mul }
| SLASH { Div }
| MOD   { Mod }

| LE    { Le  }
| LT    { Lt  }
| GE    { Ge  }
| GT    { Gt  }
| EQ    { Eq  }
| NEQ   { Neq }

| AND   { And }
| OR    { Or  }
;

%inline uop :
| U_MINUS { Opp }

| NOT     { Not }
;