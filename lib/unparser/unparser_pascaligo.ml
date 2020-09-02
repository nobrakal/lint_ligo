open Simple_utils.Region
open Pascaligo.CST

open Ast
open Common

type cst = Pascaligo.CST.t

type node =
  | Name | Keyword
  | AttributeDeclaration | TypeDeclaration | ConstDeclaration | FunDeclaration | VarDeclaration
  | Par | Brace | Bracket
  | Type | TVariant | TFieldDecl
  | Expr
  | Instruction
  | Param
  | Case
  | Pattern
  | Block
  | WhileLoop | ForLoop
  | Binding | FieldPathAssignement | FieldAssignment

let string_of_node = function
  | Name -> "name"
  | Keyword -> "keyword"
  | AttributeDeclaration -> "attrdecl"
  | TypeDeclaration -> "typedecl"
  | ConstDeclaration -> "constdecl"
  | FunDeclaration -> "fundecl"
  | VarDeclaration -> "vardecl"
  | Par -> "par"
  | Brace -> "brace"
  | Bracket -> "bracket"
  | Type -> "type"
  | TVariant -> "tvariant"
  | TFieldDecl -> "tfielddecl"
  | Expr -> "expr"
  | Instruction -> "instruction"
  | Param -> "param"
  | Case -> "case"
  | Pattern -> "pattern"
  | Block -> "block"
  | WhileLoop -> "whileloop"
  | ForLoop -> "forloop"
  | Binding -> "binding"
  | FieldPathAssignement -> "fieldpathassignement"
  | FieldAssignment -> "fieldassignment"

let node_of_string = function
  | "name" -> Some Name
  | "keyword" -> Some Keyword
  | "attrdecl" -> Some AttributeDeclaration
  | "typedecl" -> Some TypeDeclaration
  | "constdecl" -> Some ConstDeclaration
  | "fundecl" -> Some FunDeclaration
  | "vardecl" -> Some VarDeclaration
  | "par" -> Some Par
  | "brace" -> Some Brace
  | "bracket" -> Some Bracket
  | "type" -> Some Type
  | "tvariant" -> Some TVariant
  | "tfielddecl" -> Some TFieldDecl
  | "expr" -> Some Expr
  | "instruction" -> Some Instruction
  | "param" -> Some Param
  | "case" -> Some Case
  | "pattern" -> Some Pattern
  | "block" -> Some Block
  | "whileloop" -> Some WhileLoop
  | "forloop" -> Some ForLoop
  | "binding" -> Some Binding
  | "fieldpathassignement" -> Some FieldPathAssignement
  | "fieldassignment" -> Some FieldAssignment
  | _ -> None

type ast = node Ast.t

let lex x = node Name [Ast_lex x]

let rlex x = lex x.value x.region

module K = struct
  let kwd x = node Keyword [Ast_lex x]

  let kwd_end       = kwd "end"
  let kwd_type      = kwd "type"
  let kwd_is        = kwd "is"
  let kwd_const     = kwd "const"
  let kwd_recursive = kwd "recursive"
  let kwd_function  = kwd "function"
  let kwd_var       = kwd "var"
  let kwd_if        = kwd "if"
  let kwd_then      = kwd "then"
  let kwd_else      = kwd "else"
  let kwd_skip      = kwd "skip"
  let kwd_begin     = kwd "begin"
  let kwd_and       = kwd "and"
  let kwd_or        = kwd "or"
  let kwd_not       = kwd "not"
  let kwd_mod       = kwd "mod"
  let kwd_while     = kwd "while"
  let kwd_for       = kwd "for"
  let kwd_to        = kwd "to"
  let kwd_step      = kwd "step"
  let kwd_in        = kwd "in"
  let kwd_set       = kwd "set"
  let kwd_map       = kwd "map"
  let kwd_list      = kwd "list"
  let kwd_with      = kwd "with"
  let kwd_nil       = kwd "nil"
  let kwd_contains  = kwd "contains"
  let kwd_patch     = kwd "patch"
  let kwd_from      = kwd "from"
  let kwd_remove    = kwd "remove"
  let kwd_of        = kwd "of"
  let kwd_case      = kwd "case"

  let block = kwd "block"

  (* Symbols *)

  let semi     = kwd ";"
  let comma    = kwd ","
  let lpar     = kwd "("
  let rpar     = kwd ")"
  let lbrace   = kwd "{"
  let rbrace   = kwd "}"
  let lbracket = kwd "["
  let rbracket = kwd "]"
  let code_inj = kwd "[%"
  let cons     = kwd "#"
  let vbar     = kwd "|"
  let arrow    = kwd "->"
  let assign   = kwd ":="
  let equal    = kwd "="
  let colon    = kwd ":"
  let lt       = kwd "<"
  let leq      = kwd "<="
  let gt       = kwd ">"
  let geq      = kwd ">="
  let neq      = kwd "=/="
  let plus     = kwd "+"
  let minus    = kwd "-"
  let slash    = kwd "/"
  let times    = kwd "*"
  let dot      = kwd "."
  let wild     = kwd "_"
  let cat      = kwd "^"

  let dquote  = kwd "\""
  let lverbat = kwd "{|"
  let rverbat = kwd "|}"

  let c_Unit  = kwd "Unit"
  let c_True  = kwd "True"
  let c_False = kwd "False"
  let c_Some  = kwd "Some"
  let c_None  = kwd "None"

  let big_map    = kwd "big_map"
  let attributes = kwd "attributes"
  let record     = kwd "record"
end

let print_par f x =
  let {lpar;inside;rpar} = x.value in
  Ast_node (x.region, Par, K.lpar lpar :: f inside @ [K.rpar rpar])

let print_braces f x =
  let {lbrace;inside;rbrace} = x.value in
  Ast_node (x.region, Brace, K.lbrace lbrace :: f inside @ [K.rbrace rbrace])

let print_bracket f x =
  let {lbracket;inside;rbracket} = x.value in
  Ast_node (x.region, Bracket, K.lbracket lbracket :: f inside @ [K.rbrace rbracket])

let print_injection_kwd = function
  | InjSet    x -> K.kwd_set x
  | InjMap    x -> K.kwd_map x
  | InjBigMap x -> K.big_map x
  | InjList   x -> K.kwd_list x

let print_ne_injection_kwd = function
  | NEInjAttr   x -> K.attributes x
  | NEInjSet    x -> K.kwd_set x
  | NEInjMap    x -> K.kwd_map x
  | NEInjRecord x -> K.record x

let print_collection = function
  | Map  x -> K.kwd_map x
  | Set  x -> K.kwd_set x
  | List x -> K.kwd_list x

let print_enclosing = function
  | Brackets (l,r) -> (Some (K.lbracket l), K.rbracket r)
  | End x          -> (None, K.kwd_end x)

let print_terminator =  opt_to_list (fun x -> [K.semi x])

let print_ne_injection : 'a. ('a -> ast) -> ('a ne_injection) -> ast list =
  fun f {kind; enclosing; ne_elements; terminator} ->
  let (opening,closing) = print_enclosing enclosing in
  let elements = print_nsepseq K.semi f ne_elements in
  print_ne_injection_kwd kind
  :: opt_to_list (fun x -> [x]) opening @ elements @ print_terminator terminator @ [closing]

let print_attr_decl xs =
  node AttributeDeclaration (print_ne_injection rlex xs.value) xs.region

(* Type *)
let rec print_type_expr reg x =
  let xs = match x with
    | TProd   x -> print_tprod x
    | TSum    x -> print_tsum x
    | TRecord x -> print_trecord x
    | TApp    x -> print_tapp x
    | TFun    x -> print_tfun x
    | TPar    x -> [print_par (fun e -> [print_type_expr x.region e]) x]
    | TVar    x -> [rlex x] (* TODO *)
    | TWild   x -> [K.wild x]
    | TString x -> [rlex x] in
  node Type xs reg

and print_tapp x =
  let (constr,arg) = x.value in
  [rlex constr; print_par (print_nsepseq K.comma (print_type_expr x.region) ) arg]

and print_trecord x =
  print_ne_injection print_field_decl x.value

and print_field_decl x =
  let {field_name; colon; field_type} = x.value in
  let xs = [rlex field_name; K.colon colon; print_type_expr x.region field_type] in
  node TFieldDecl xs x.region

and print_tfun x =
  let (l,arrow,r) = x.value in
  [print_type_expr x.region l; K.arrow arrow; print_type_expr x.region r]

and print_tsum x =
  print_nsepseq K.vbar print_variant x.value

and print_variant x =
  let {constr;arg} = x.value in
  let xs =
    rlex constr :: opt_to_list (fun (kwd_of,t) -> [K.kwd_of kwd_of; print_type_expr x.region t]) arg in
  node TVariant xs x.region

and print_tprod x =
  print_nsepseq K.times (print_type_expr x.region) x.value

let print_type_opt reg x = opt_to_list (fun (c,t) -> [K.colon c; print_type_expr reg t]) x

let print_type_decl x =
  let {kwd_type; name; kwd_is; type_expr; terminator} = x.value in
  let xs =
    K.kwd_type kwd_type :: rlex name :: K.kwd_is kwd_is :: print_type_expr x.region type_expr ::
      opt_to_list (fun x -> [K.semi x]) terminator in
  node TypeDeclaration xs x.region

(* Expr *)
let rec print_expr reg x =
  let xs = match x with
      ECase    x -> print_case print_expr x
    | ECond    x -> print_cond_expr x
    | EAnnot   x -> print_annot reg x
    | ELogic   x -> print_logic_expr x
    | EArith   x -> print_arith_expr x
    | EString  x -> print_string_expr x
    | EList    x -> print_list_expr x
    | ESet     x -> print_set_expr x
    | EConstr  x -> print_constr x
    | ERecord  x -> print_record x
    | EProj    x -> print_projection x
    | EUpdate  x -> print_update x
    | EMap     x -> print_map_expr x
    | EVar     x -> [rlex x]
    | ECall    x -> print_fun_call x
    | EBytes   x -> [print_bytes lex x]
    | EUnit    x -> [K.c_Unit x]
    | ETuple   x -> [print_par (print_nsepseq K.comma (print_expr x.region)) x]
    | EPar     x -> [print_par (fun e -> [print_expr x.region e]) x]
    | EFun     x -> print_fun_expr x
    | ECodeInj x -> print_code_inj x
    | EBlock   x -> print_block_with x
  in node Expr xs reg

and print_annot reg x =
  [print_par (fun (e,c,t) -> [print_expr reg e; K.colon c; print_type_expr reg t]) x]

and print_record x = print_ne_injection print_field_assigment x.value

and print_constr = function
  | SomeApp x ->
     let (s,args) = x.value in
     [K.c_Some s; print_expr x.region (ETuple args)]
  | NoneExpr n ->
     [K.c_None n]
  | ConstrApp x ->
     let (constr,args) = x.value in
     rlex constr :: opt_to_list (fun args -> [print_expr x.region (ETuple args)]) args

and print_set_membership x =
  let {set;kwd_contains;element} = x.value in
  [print_expr x.region set; K.kwd_contains kwd_contains; print_expr x.region element]

and print_set_expr = function
  | SetInj x -> print_injection (print_expr x.region) x
  | SetMem x -> print_set_membership x

and print_list_expr = function
  | ECons x -> print_bin_op K.cons x
  | EListComp x -> print_injection (print_expr x.region) x
  | ENil x -> [K.kwd_nil x]

and print_cond_expr x =
  let {kwd_if; test; kwd_then; ifso; terminator; kwd_else; ifnot} : cond_expr = x.value in
  [K.kwd_if kwd_if; print_expr x.region test; K.kwd_then kwd_then; print_expr x.region ifso]
  @ print_terminator terminator
  @ [K.kwd_else kwd_else; print_expr x.region ifnot]

and print_field_assigment x =
  let {field_name; assignment; field_expr} = x.value in
  let xs = [rlex field_name; K.assign assignment; print_expr x.region field_expr] in
  node FieldAssignment xs x.region

and print_binding x =
  let {source;arrow;image} = x.value in
  node Binding [print_expr x.region source; K.arrow arrow; print_expr x.region image] x.region

and print_update x =
  let {record;kwd_with;updates} = x.value in
  print_path record
  @ [K.kwd_with kwd_with]
  @ print_ne_injection print_field_path_assignment updates.value

and print_field_path_assignment x =
  let {field_path; assignment; field_expr} = x.value in
  let xs = print_path field_path @ [K.equal assignment; print_expr x.region field_expr]  in
  node FieldPathAssignement xs x.region

and print_injection : 'a. ('a -> ast) -> 'a injection reg -> ast list = fun f x ->
  let {kind;enclosing;elements;terminator} = x.value in
  let opening,closing = print_enclosing enclosing in
  print_injection_kwd kind
  :: opt_to_list (fun x -> [x]) opening
  @ print_sepseq K.semi f elements
  @ [closing]
  @ print_terminator terminator

and print_map_expr = function
  | MapLookUp x -> print_map_lookup x
  | MapInj x -> print_injection print_binding x
  | BigMapInj x -> print_injection print_binding x

and print_fun_call x =
  let (e,args) = x.value in
  [print_expr x.region e; print_expr x.region (ETuple args)]

and print_string_expr = function
  | Cat e -> print_bin_op K.cat e
  | String e -> [K.dquote e.region; rlex e; K.dquote e.region]
  | Verbatim e -> [K.lverbat e.region; rlex e; K.rverbat e.region]

and print_code_inj x =
  let {language; code;rbracket} = x.value in
  [K.code_inj language.region;
   lex ("<" ^ language.value.value ^ ">") language.value.region; print_expr x.region code; K.rbracket rbracket]

and print_fun_expr x =
  let {kwd_function; param; ret_type; kwd_is; return} : fun_expr = x.value in
  [K.kwd_function kwd_function; print_parameters param]
  @ print_type_opt x.region ret_type
  @ [K.kwd_is kwd_is; print_expr x.region return]

and print_block_with x =
  let {block;kwd_with;expr} = x.value in
  [print_block block; K.kwd_with kwd_with; print_expr x.region expr]

and print_logic_expr = function
  | BoolExpr x -> print_bool_expr x
  | CompExpr x -> print_comp_expr x

and print_bin_op fop x =
  let {arg1;arg2; op} = x.value in
  [print_expr x.region arg1; fop op; print_expr x.region arg2]

and print_un_op fop x =
  let {arg;op} = x.value in
  [fop op; print_expr x.region arg]

and print_bool_expr = function
    Or   e  -> print_bin_op K.kwd_or e
  | And  e  -> print_bin_op K.kwd_and e
  | Not  e  -> print_un_op K.kwd_not e
  | True  t -> [K.c_True t]
  | False f -> [K.c_False f]

and print_comp_expr = function
  | Lt    e -> print_bin_op K.lt    e
  | Leq   e -> print_bin_op K.leq   e
  | Gt    e -> print_bin_op K.gt    e
  | Geq   e -> print_bin_op K.geq   e
  | Equal e -> print_bin_op K.equal e
  | Neq   e -> print_bin_op K.neq   e

and print_arith_expr = function
    Add   e -> print_bin_op K.plus    e
  | Sub   e -> print_bin_op K.minus   e
  | Mult  e -> print_bin_op K.times   e
  | Div   e -> print_bin_op K.slash   e
  | Mod   e -> print_bin_op K.kwd_mod e
  | Neg   e -> print_un_op K.minus    e
  | Int   e -> [print_int lex e]
  | Nat   e -> [print_nat lex e]
  | Mutez e -> [print_mutez lex e]

and print_instruction reg x =
  let xs = match x with
    | Cond       x -> print_conditional x
    | CaseInstr  x -> print_case print_if_clause x
    | Assign     x -> print_assignement x
    | Loop       x -> [print_loop x]
    | ProcCall   x -> print_fun_call x
    | Skip       x -> [K.kwd_skip x]
    | RecordPatch x -> print_record_patch x
    | MapPatch    x -> print_map_patch x
    | SetPatch    x -> print_set_patch x
    | MapRemove   x -> print_map_remove x
    | SetRemove   x -> print_set_remove x
  in node Instruction xs reg

and print_set_remove x =
  let {kwd_remove; element; kwd_from; kwd_set; set} = x.value in
  [K.kwd_remove kwd_remove; print_expr x.region element; K.kwd_from kwd_from
   ; K.kwd_set kwd_set] @ print_path set

and print_map_remove x =
  let {kwd_remove; key; kwd_from; kwd_map; map} = x.value in
  [K.kwd_remove kwd_remove; print_expr x.region key; K.kwd_from kwd_from
  ; K.kwd_map kwd_map] @ print_path map

and print_map_patch x =
  let {kwd_patch; path; kwd_with; map_inj} = x.value in
  [K.kwd_patch kwd_patch] @ print_path path @ [K.kwd_with kwd_with]
  @ print_ne_injection print_binding map_inj.value

and print_set_patch x =
  let {kwd_patch; path; kwd_with; set_inj} = x.value in
  [K.kwd_patch kwd_patch] @ print_path path @ [K.kwd_with kwd_with]
  @ print_ne_injection (print_expr x.region) set_inj.value

and print_record_patch x =
  let {kwd_patch; path; kwd_with; record_inj} = x.value in
  [K.kwd_patch kwd_patch] @ print_path path @ [K.kwd_with kwd_with] @ print_record record_inj

and print_selection = function
  | FieldName x -> rlex x
  | Component x -> print_int lex x

and print_projection x =
  let {struct_name; selector; field_path} = x.value in
  [rlex struct_name; K.dot selector] @ print_nsepseq K.dot print_selection field_path

and print_path = function
  | Pascaligo.CST.Name x -> [rlex x]
  | Path xs -> print_projection xs

and print_map_lookup x =
  let {path; index} = x.value in
  print_path path @ [print_bracket (fun e -> [print_expr x.region e]) index]

and print_lhs : lhs -> ast list = function
  | Path x -> print_path x
  | MapPath x -> print_map_lookup x

and print_assignement x =
  let {lhs;assign;rhs} = x.value in
  print_lhs lhs @ [K.assign assign; print_expr x.region rhs]

and print_conditional x =
  let {kwd_if; test; kwd_then; ifso; terminator; kwd_else; ifnot} : conditional = x.value in
  [K.kwd_if kwd_if; print_expr x.region test; K.kwd_then kwd_then; print_if_clause x.region ifso]
  @ print_terminator terminator
  @ [K.kwd_else kwd_else; print_if_clause x.region ifnot]

and print_if_clause reg = function
  | ClauseInstr inst -> print_instruction reg inst
  | ClauseBlock block -> print_clause_block block

and print_var_decl x =
  let {kwd_var; name; var_type; assign; init; terminator} = x.value in
  let xs =
    [K.kwd_var kwd_var; rlex name]
    @ print_type_opt x.region var_type
    @ [K.assign assign; print_expr x.region init]
    @ print_terminator terminator in
  node VarDeclaration xs x.region

and print_data = function
  | LocalConst x -> print_const_decl x
  | LocalVar   x -> print_var_decl x
  | LocalFun   x -> print_fun_decl x

and print_statement reg = function
  | Instr x -> print_instruction reg x
  | Data  x -> print_data x
  | Attr  x -> print_attr_decl x

and print_statements reg x =
  print_nsepseq K.semi (print_statement reg) x

and print_block x =
  let {enclosing; statements; terminator} = x.value in
  let opening,closing =
    match enclosing with
    | Block (block,lbrace,rbrace) -> [K.block block; K.lbrace lbrace], K.rbrace rbrace
    | BeginEnd (kwd_begin,kwd_end) -> [K.kwd_begin kwd_begin], K.kwd_end kwd_end in
  let xs = opening @ print_statements x.region statements @ closing :: print_terminator terminator in
  node Block xs x.region

and print_clause_block = function
  | LongBlock  x ->
     print_block x
  | ShortBlock x ->
     print_braces (fun (s,t) -> print_statements x.region s @ print_terminator t) x

(* Loops *)
and print_loop = function
  | While x -> print_while_loop x
  | For (ForInt x) -> print_for_int x
  | For (ForCollect x) -> print_for_collect x

and print_while_loop x =
  let {kwd_while;cond;block} = x.value in
  let xs = [K.kwd_while kwd_while; print_expr x.region cond; print_block block] in
  node WhileLoop xs x.region

and print_for_int x =
  let {kwd_for; binder; assign; init; kwd_to; bound; step; block} = x.value in
  let xs =
    [K.kwd_for kwd_for; rlex binder; K.assign assign;
     print_expr x.region init; K.kwd_to kwd_to; print_expr x.region bound]
    @ opt_to_list (fun (s,e) -> [K.kwd_step s; print_expr x.region e]) step
    @ [print_block block]
  in node ForLoop xs x.region

and print_for_collect x =
  let {kwd_for; var; bind_to; kwd_in; collection; expr; block} = x.value in
  let xs =
    [K.kwd_for kwd_for; rlex var]
    @ opt_to_list (fun (a,x) -> [K.arrow a; rlex x]) bind_to
    @ [K.kwd_in kwd_in; print_collection collection; print_expr x.region expr; print_block block]
  in node ForLoop xs x.region

(* Pattern *)
and print_case : 'a. (Region.t -> 'a -> ast) -> 'a case reg -> ast list = fun f x ->
  let {kwd_case; expr; kwd_of; enclosing; lead_vbar; cases} = x.value in
  let (opening, closing) = print_enclosing enclosing in
  [K.kwd_case kwd_case; print_expr x.region expr; K.kwd_of kwd_of]
  @ opt_to_list (fun x -> [x]) opening
  @ opt_to_list (fun x -> [K.vbar x]) lead_vbar
  @ print_nsepseq K.vbar (print_case_clause f) cases.value
  @ [closing]

and print_case_clause : 'a. (Region.t -> 'a -> ast) -> 'a case_clause reg -> ast = fun f x ->
  let {pattern; arrow; rhs} = x.value in
  node Case [print_pattern x.region pattern; K.arrow arrow; f x.region rhs] x.region

and print_pattern reg x =
  let xs = match x with
    | PConstr x -> print_constr_pattern x
    | PVar    x -> [rlex x]
    | PWild   x -> [K.wild x]
    | PInt    x -> [print_int lex x]
    | PNat    x -> [print_nat lex x]
    | PBytes  x -> [print_bytes lex x]
    | PString x -> [K.dquote x.region; rlex x; K.dquote x.region]
    | PList   x -> print_pattern_list x
    | PTuple  x -> [print_par (print_nsepseq K.comma (print_pattern x.region)) x]
  in node Pattern xs reg

and print_pattern_list = function
  | PListComp x -> print_injection (print_pattern x.region) x
  | PNil      x -> [K.kwd_nil x]
  | PParCons  x -> [print_par (fun (y,c,ys) -> [print_pattern x.region y; K.cons c; print_pattern x.region ys]) x]
  | PCons     x -> print_nsepseq K.cons (print_pattern x.region) x.value

and print_constr_pattern = function
  | PUnit      x -> [K.c_Unit x]
  | PFalse     x -> [K.c_False x]
  | PTrue      x -> [K.c_True x]
  | PNone      x -> [K.c_None x]
  | PSomeApp   x ->
     let (s,arg) = x.value in
     [K.c_Some s; print_par (fun p -> [print_pattern x.region p]) arg]
  | PConstrApp x ->
     let (constr,arg) = x.value in
     rlex constr
     :: opt_to_list (fun x -> [print_par (print_nsepseq K.comma (print_pattern x.region)) x]) arg

and print_const_decl x =
  let {kwd_const; name; const_type; equal; init; terminator; attributes} = x.value in
  let xs =
    K.kwd_const kwd_const :: rlex name
    :: print_type_opt x.region const_type
    @ [K.equal equal; print_expr x.region init]
    @ print_terminator terminator
    @ opt_to_list (fun x -> [print_attr_decl x]) attributes
  in node ConstDeclaration xs x.region

and print_param_decl x =
  let (kwd,var,param_type,reg) = match x with
    | ParamConst x ->
       let {kwd_const; var; param_type} = x.value in
       (K.kwd_const kwd_const, var, param_type, x.region)
    | ParamVar x ->
       let {kwd_var; var; param_type} = x.value in
       (K.kwd_var kwd_var, var, param_type, x.region) in
  let xs = [kwd; rlex var] @ print_type_opt reg param_type in
  node Param xs reg

and print_parameters x =
  print_par (fun xs -> print_nsepseq K.semi print_param_decl xs) x

and print_fun_decl x =
  let {kwd_recursive; kwd_function; fun_name; param; ret_type; kwd_is; return; terminator; attributes} = x.value in
  let xs =
    opt_to_list (fun x -> [K.kwd_recursive x]) kwd_recursive
    @ [K.kwd_function kwd_function; rlex fun_name; print_parameters param]
    @ print_type_opt x.region ret_type
    @ [K.kwd_is kwd_is; print_expr x.region return]
    @ print_terminator terminator
    @ opt_to_list (fun x -> [print_attr_decl x]) attributes
  in node FunDeclaration xs x.region

let print_declaration = function
  | TypeDecl  td -> print_type_decl td
  | ConstDecl cd -> print_const_decl cd
  | FunDecl   fd -> print_fun_decl fd
  | AttrDecl  ad -> print_attr_decl ad

let unparse_cst : Pascaligo.CST.t -> node Ast.t list =
  fun cst -> List.map print_declaration (Utils.nseq_to_list cst.decl)
