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
%token INTERO TWO_PT
%token IF ELSE
%token DO WHILE FOR

%token MAIN
%token PRINT
%token EOF

%token NEW CLASS EXTENDS
%token ATTR DOT THIS
%token METHOD COMMA RETURN



%right INTERO TWO_PT

%left OR
%left AND
%left EQ NEQ
%left LE LT GE GT
%nonassoc NOT

%left PLUS MINUS 
%left TIMES SLASH MOD
%nonassoc U_MINUS

%right NEW

%left DOT COMMA



%start program
%type <Kawa.program> program

%%

program:
| var=list(variable) cls=list(class_def) MAIN BEGIN body=list(instruction) END EOF
    { { classes = cls ; globals = var ; main = body } }
;


class_def:
| CLASS i=IDENT BEGIN attr=list(attribute) meth=list(methods) END
    { { class_name = i ; attributes = attr ; methods = meth ; parent = None } }
| CLASS i=IDENT EXTENDS p=IDENT BEGIN attr=list(attribute) meth=list(methods) END
    { { class_name = i ; attributes = attr ; methods = meth ; parent = Some(p) } }
;


variable:
| VAR t=TYPE i=IDENT SEMI { (i, t) }
;
attribute:
| ATTR t=TYPE i=IDENT SEMI { (i, t) }
;


methods:
| METHOD t=TYPE i=IDENT LPAR args=arg RPAR BEGIN var=list(variable) body=list(instruction) END 
    { { method_name = i ; code = body ; params = args ; locals = var ; return = t } }
;
arg:
| t=TYPE i=IDENT             { [ (i, t) ]  }
| t=TYPE i=IDENT COMMA a=arg { (i, t) :: a }
;


expression:
| n=INT                                   { Int(n)            }
| b=BOOL                                  { Bool(b)           }

| THIS                                    { This              }
| m=mem_access                            { Get(m)            }

| op=uop e=expression                     { Unop(op, e)       }
| e1=expression op=bop e2=expression      { Binop(op, e1, e2) }

| LPAR e=expression RPAR                  { e                 }

| NEW i=IDENT                             { New(i)            }
| NEW i=IDENT LPAR e=expression_list RPAR { NewCstr(i, e)     }

| e=expression DOT i=IDENT LPAR el=expression_list RPAR 
    { MethCall(e, i, el) }

| e1=expression INTERO e2=expression TWO_PT e3=expression
    { TerCond(e1, e2, e3) }
;
expression_list:
| e=expression                          { [ e ]   }
| e=expression COMMA el=expression_list { e :: el }


mem_access:
| i=IDENT                   { Var(i)      }
| e=expression DOT i=IDENT  { Field(e, i) }
;


instruction:
| PRINT LPAR e=expression RPAR SEMI                             
    { Print(e) }
| m=mem_access SET e=expression SEMI                                 
    { Set(m, e) }
| c=condition                                                   
    { Cond(c) }
| WHILE LPAR e=expression RPAR BEGIN body=list(instruction) END
    { While(e, body) }
| DO BEGIN body=list(instruction) END WHILE LPAR e=expression RPAR SEMI
    { DoWhile(body, While(e, body)) }
| RETURN e=expression SEMI
    { Return(e) }
| e=expression SEMI
    { Expr(e) }

;
condition:
| IF LPAR e=expression RPAR BEGIN body=list(instruction) END
    { If(e, body) }
| IF LPAR e=expression RPAR BEGIN body1=list(instruction) END ELSE BEGIN body2=list(instruction) END
    { If_Else(e, body1, Else(body2)) }
| IF LPAR e=expression RPAR BEGIN body=list(instruction) END ELSE c=condition
    { If_Else(e, body, c) }
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