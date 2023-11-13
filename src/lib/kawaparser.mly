%{

  open Lexing
  open Kawa
%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token <Kawa.typ> TYPE

%token LPAR RPAR BEGIN END SEMI SET

%token U_MINUS
%token PLUS MINUS TIMES SLASH MOD

%token NOT AND OR
%token LE LT GE GT EQ NEQ

%token VAR
%token IF ELSE

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
| var=list(variable) MAIN BEGIN body=list(instruction) END EOF
    { { classes = [] ; globals = var ; main = body } }
;

variable:
| VAR t=TYPE i=IDENT SEMI { (i, t) }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e)       }
| i=IDENT SET e=expression SEMI     { Set(Var(i), e) }
| c=condition                       { Cond(c)        }
;

condition:
| IF LPAR e=expression RPAR BEGIN body=list(instruction) END
    { If(e, body) }
| IF LPAR e=expression RPAR BEGIN body1=list(instruction) END ELSE BEGIN body2=list(instruction) END
    { If_Else(e, body1, Else(body2)) }
| IF LPAR e=expression RPAR BEGIN body=list(instruction) END ELSE c=condition
    { If_Else(e, body, c) }
;

expression:
| n=INT                              { Int(n)            }
| b=BOOL                             { Bool(b)           }

| i=IDENT                            { Get(Var(i))       }

| LPAR e=expression RPAR             { e                 }

| e1=expression op=bop e2=expression { Binop(op, e1, e2) }
| op=uop e=expression                { Unop(op, e)       }
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