%{

  open Lexing
  open Kawa

  let init_and_setting_vars var =
    let rec var_set l =
    match l with
    | [] -> []
    | s :: l -> 
      begin
        match s with
        | Set _ -> s :: var_set l
        | _ -> var_set l
      end
    in
    let set = var_set (List.map snd var) in
    let var = List.flatten (List.map fst var) in
    var, set

%}

%token <int> N
%token <float> F
%token TRUE FALSE
%token NULL

%token INT FLOAT BOOL VOID
%token <string> IDENT

%token LPAR RPAR BEGIN END SEMI

%token PLUS U_MINUS MINUS TIMES SLASH MOD
%token PLUS_SET MINUS_SET TIMES_SET SLASH_SET

%token NOT AND OR
%token LE LT GE GT EQ NEQ

%token VAR SET

%token INTERO TWO_PT IF ELSE
%token DO WHILE FOR

%token NEW CLASS EXTENDS
%token THIS DOT ATTR
%token METHOD COMMA RETURN

%token MAIN
%token PRINT ASSERT
%token EOF


/* Priority from Java (I put them all, even if it's useless)

   Source :
   [https://pages.cs.wisc.edu/~willb/cs302/java-operator-precedence.pdf]
*/
%right SET PLUS_SET MINUS_SET TIMES_SET SLASH_SET

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
| var=list(var_decl) cls=list(class_def) MAIN s=seq EOF
  { 
    let var, set = init_and_setting_vars var in
    { classes = cls ; globals = var ; main = set @ s } 
  }
;
seq:
| BEGIN body=list(instr) END { body }
;



class_def:
| CLASS i=IDENT pt=option(EXTENDS p=IDENT { p }) BEGIN attr=flatten(list(attr_decl)) cstr=option(constructor_def) meth=list(method_def) END
  { 
    match cstr with
    | Some c -> { class_name = i ; attributes = attr ; methods = (c :: meth) ; parent = pt }
    | None -> { class_name = i ; attributes = attr ; methods = meth ; parent = pt } 
  }
;



var_decl:
| VAR t=typ i=IDENT SET e=expr SEMI
  { [ (i, t) ], Set(Var(i), S_Set, e) }
| VAR t=typ l=separated_nonempty_list(COMMA, i=IDENT { i }) SEMI 
  { (List.map (fun i -> (i, t) ) l), Expr(Null) }
;
attr_decl:
| ATTR t=typ l=separated_nonempty_list(COMMA, i=IDENT { i }) SEMI 
  { List.map (fun i -> (i, t) ) l }
;



typ:
| INT { TInt }
| FLOAT { TFloat }
| BOOL { TBool }
| i=IDENT { TClass i }
| VOID { TVoid }
;


constructor_def:
| i=IDENT LPAR arg=separated_list(COMMA, t=typ i=IDENT { (i, t) }) RPAR BEGIN var=list(var_decl) body=list(instr) END
  {
    let var, set = init_and_setting_vars var in
    { method_name = i ; code = set @ body ; params = arg ; locals = var ; return = TVoid } 
  }
;
method_def:
| METHOD t=typ i=IDENT LPAR arg=separated_list(COMMA, t=typ i=IDENT { (i, t) }) RPAR BEGIN var=list(var_decl) body=list(instr) END
  { 
    let var, set = init_and_setting_vars var in
    { method_name = i ; code = set @ body ; params = arg ; locals = var ; return = t } 
  }
;



expr:
| n=N { Int(n) }
| f=F { Float(f) }

| TRUE { Bool(true) }
| FALSE { Bool(false) }

| NULL { Null }

| u=uop e=expr { Unop(u, e) }
| e1=expr b=bop e2=expr { Binop(b, e1, e2) }

| t=expr INTERO e1=expr TWO_PT e2=expr { TerCond(t, e1, e2) }

| m=mem { Get(m) }

| THIS { This }

| LPAR e=expr RPAR { e }

| NEW i=IDENT { New i }
| NEW i=IDENT LPAR arg=separated_list(COMMA, expr) RPAR 
  { NewCstr(i, arg) }

| e=expr DOT i=IDENT LPAR arg=separated_list(COMMA, expr) RPAR
  { MethCall(e, i, arg) }
;
mem:
| i=IDENT { Var i }
| e=expr DOT i=IDENT { Field(e, i) }
;



instr:
| PRINT LPAR e=expr RPAR SEMI { Print e }
| ASSERT LPAR e=expr RPAR SEMI { Assert e }

| m=mem s=set e=expr SEMI { Set(m, s, e) }

| c=cond { Cond(c) }

| WHILE LPAR e=expr RPAR s=seq { While(e, s) }
| DO s=seq WHILE LPAR e=expr RPAR SEMI { DoWhile(s, While(e, s)) }

| RETURN e=expr SEMI { Return e }

| e=expr SEMI { Expr e }
;
cond:
| IF LPAR e=expr RPAR s=seq { If(e, s) }
| IF LPAR e=expr RPAR s1=seq ELSE s2=seq { If_Else(e, s1, Else(s2)) }
| IF LPAR e=expr RPAR s=seq ELSE c=cond { If_Else(e, s, c) }
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

%inline set:
| SET       { S_Set }
| PLUS_SET  { S_Add }
| MINUS_SET { S_Sub }
| TIMES_SET { S_Mul }
| SLASH_SET { S_Div }