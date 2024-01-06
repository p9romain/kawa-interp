(**
   Kawa : a small object language inspired by Java
 *)
(* Declared types (used by attributes, variables, args and return type in methods. *)
type typ =
  | TVoid
  | TInt
  | TFloat
  | TString
  | TBool
  | TClass of string
  | TTab of typ

let rec typ_to_string (t : typ) : string =
  match t with
  | TVoid -> "void"
  | TInt -> "int"
  | TFloat -> "float"
  | TString -> "string"
  | TBool -> "bool"
  | TClass s -> s
  | TTab t -> (typ_to_string t) ^ "[]"

(* Operators *)
type unop  = Opp | Not
type binop = Add | Sub | Mul | Div | Mod
           | Le  | Lt  | Ge | Gt | Eq  | Neq
           | And | Or
type setop = S_Set | S_Sub | S_Add | S_Mul | S_Div

(* Expressions *)
type expr =
  (* Constant (and some casting) *)
  | Int        of int
  (* | IntCast    of expr *)
  | Float      of float
  (* | FloatCast  of expr *)
  | String     of string
  | Bool       of bool
  | Null
  (* Operators *)
  | Unop       of unop * expr
  | Binop      of binop * expr * expr
  | TerCond    of expr * expr * expr
  (* Check if an object is an instance of a certain class *)
  | InstanceOf of expr * typ
  (* Get the content of a var or an attributte *)
  | Get        of mem_access
  (* Current object *)
  | This
  (* Create a new object *)
  | New        of string
  | NewCstr    of string * expr list
  (* Call a method *)
  | MethCall   of expr * string * expr list

(* Memory access : either a variable or an attribute *)
and mem_access =
  | Var    of string
  | Field  of expr (* object *) * string (* attribute's name *)

(* Instructions *)
type instr =
  (* Print a boolean or an int *)
  | Print  of expr
  (* Check is something is true : else it terminates the program *)
  | Assert of expr
  (* Set the content in a variable or an attribute *)
  | Set    of mem_access * setop * expr
  (* Condition control *)
  | Cond   of cond
  (* Loops *)
  | For     of typ option * instr * expr * instr * seq
  | While   of expr * seq
  | DoWhile of seq * instr
  (* End of a method *)
  | Return of expr
  (* Expression *)
  | Expr   of expr
and cond =
  | If        of expr * seq
  | If_Else   of expr * seq * cond
  | Else      of seq
and seq = instr list

(* Method's definition

   Syntax : method <return type> <name> (<params>) { ... }

   Method's body is similar to the main's body *)
type method_def = {
    method_name : string ;
    code : seq ;
    (* can't change into a Hashtbl because it needs to be ordered 
      (and the params are an expr list so it keeps the order) *)
    params : (string * typ) list ;
    locals : (string, typ) Hashtbl.t ;
    return : typ ;
  }
        
(* Class's definition

   Syntax : class <class name> { ... }
       or : class <class name> extends <mother class name> { ... }

   The class's constructor is a void-typed method called "constructor" *)
type class_def = {
    class_name : string ;
    attributes : (string, typ) Hashtbl.t ;
    methods : (string, method_def) Hashtbl.t ;
    parent : string option ;
  }

(* Program : global variables, classes, inheritances and a sequence of instruction (the main) *)
type program = {
    classes : (string, class_def) Hashtbl.t ;
    globals : (string, typ) Hashtbl.t ;
    main : seq ;
  }
