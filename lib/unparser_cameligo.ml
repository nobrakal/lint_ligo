open Simple_utils.Region
open Cameligo.CST
open Pattern

type node =
  | Ndeclarations
  | NLet | NTypeDecl
  | NExpr
  | Par
  | Attribute
  | TCartesian | TArrow | TSum | TRecord | TTuple | TApp | TVariant
  | Pattern
  | Field | Field_assign | Field_decl
  | Path | Clause
  | Name

type nonrec ast = node ast

let lex x = Ast_node (Name,[Ast_lex x])

module K = struct
  let kwd_begin = lex "begin"
  let kwd_else  = lex "else"
  let kwd_end   = lex "end"
  let kwd_false = lex "false"
  let kwd_fun   = lex "fun"
  let kwd_rec   = lex "rec"
  let kwd_if    = lex "if"
  let kwd_in    = lex "in"
  let kwd_let   = lex "let"
  let kwd_match = lex "match"
  let kwd_mod   = lex "mod"
  let kwd_not   = lex "not"
  let kwd_of    = lex "of"
  let kwd_then  = lex "then"
  let kwd_true  = lex "true"
  let kwd_type  = lex "type"
  let kwd_with  = lex "with"

  (* Data constructors *)

  let c_None  = lex "None"
  let c_Some  = lex "Some"

  (* Symbols *)

  let arrow    = lex "->"
  let cons     = lex "::"
  let cat      = lex "^"
  let dot      = lex "."

  (* Arithmetic operators *)

  let minus    = lex "-"
  let plus     = lex "+"
  let slash    = lex "/"
  let times    = lex "*"

  (* Boolean operators *)

  let bool_or  = lex "||"
  let bool_and = lex "&&"

  (* Comparisons *)

  let equal = lex "="
  let neq   = lex "<>"
  let lt    = lex "<"
  let gt    = lex ">"
  let leq   = lex "<="
  let geq   = lex ">="

  (* Compounds *)

  let lpar     = lex "("
  let rpar     = lex ")"
  let lbracket = lex "["
  let rbracket = lex "]"
  let lbrace   = lex "}"
  let rbrace   = lex "{"
  let dquote   = lex "\""
  let lverbat  = lex "{|"
  let rverbat  = lex "|}"
  let code_inj = lex "[%"

  (* Separators *)

  let comma = lex ","
  let semi  = lex ";"
  let vbar  = lex "|"
  let colon = lex ":"

  (* Wildcard *)

  let wild = lex "_"

end

let unreg f x = f x.value

let node t xs = Ast_node (t,xs)

let par x = Ast_node (Par, K.lpar :: x @ [K.rpar])

let print_par f value =
  par (f value.inside)

let const x _ = x

let opt_to_list x = Option.fold ~none:[] ~some:(fun x -> x) x

let print_nsepseq f g (x,xs) =
  g x :: List.(concat (map (fun (x,y) -> [f x; g y]) xs))

let print_sepseq f g xs =
  match xs with
  | None -> []
  | Some xs -> print_nsepseq f g xs

let print_attribute x = node Attribute ([lex "[@@"; unreg lex x; lex "]"])
let print_attributes xs = List.map print_attribute xs

let print_int (_,value) =
  lex (Z.to_string value)

let print_nat (_,value) =
  lex (Z.to_string value ^ "n")

let print_mutez (_,value) =
  lex (Z.to_string value ^ "mutez")

let print_unit = [K.rpar;K.rpar]

let rec print_let_decl (kwd_rec, let_binding, attributes) =
  node NLet @@
    [K.kwd_let]
    @ List.map (const K.kwd_rec) (Option.to_list kwd_rec)
    @ print_let_binding let_binding
    @ print_attributes attributes

and print_pattern x =
  let xs = match x with
      PConstr   p -> print_pconstr p
    | PUnit     _ -> print_unit
    | PVar      v -> [unreg lex v]
    | PInt      i -> [unreg print_int i]
    | PNat      n -> [unreg print_nat n]
    | PBytes    b -> [print_bytes b]
    | PString   s -> print_string (String s)
    | PVerbatim s -> print_string (Verbatim s)
    | PWild     _ -> [K.wild]
    | PList     l -> print_plist l
    | PTuple    t -> unreg print_ptuple t
    | PPar      p -> unreg (fun p -> [print_par (fun x -> [print_pattern x]) p]) p
    | PRecord   r -> unreg print_precord r
    | PTyped    t -> unreg print_ptyped t in
  node Pattern xs

and print_pconstr = function
    PNone      _ -> [K.c_None]
  | PSomeApp   p -> [K.c_Some;unreg (fun (_,x) -> print_pattern x) p]
  | PFalse    _ -> [K.kwd_false]
  | PTrue     _ -> [K.kwd_true]
  | PConstrApp a -> unreg print_pconstr_app a

and print_pconstr_app = function
  | constr, None -> [unreg lex constr]
  | constr, Some pat -> [unreg lex constr; print_pattern pat]

and print_ptuple xs =
  print_nsepseq (const K.comma) print_pattern xs

and print_plist = function
  PListComp cmp -> unreg print_list_comp cmp
| PCons cons -> unreg print_pcons cons

and print_list_comp e = print_injection print_pattern e

and print_precord fields = print_ne_injection (unreg print_field_pattern) fields

and print_field_pattern {field_name; pattern; _} =
  node Field [unreg lex field_name; K.equal; print_pattern pattern]

and print_pcons (patt1, _, patt2) =
  [print_pattern patt1; K.cons; print_pattern patt2]

and print_ptyped {pattern; type_expr; _} =
  [print_pattern pattern; K.colon; print_type_expr type_expr]

and print_binders xs = List.map print_pattern (Utils.nseq_to_list xs)

and print_bytes e = lex ("0x" ^ Hex.show (snd e.value))

and print_let_binding {binders;lhs_type;let_rhs;_} =
  print_binders binders
  @ (opt_to_list (Option.map (fun (_,x) -> [K.colon; print_type_expr x]) lhs_type))
  @ [K.equal ; print_expr let_rhs]

and print_expr x =
  let ast_x : ast list =
    match x with
    | ECase       e -> unreg print_case_expr e
    | ECond       e -> unreg print_cond e
    | EAnnot      e -> unreg print_annot e
    | ELogic      e -> print_logic_expr e
    | EArith      e -> print_arith_expr e
    | EString     e -> print_string e
    | EList       e -> print_list e
    | EConstr     e -> print_constr_expr e
    | ERecord     e -> unreg (print_ne_injection (unreg print_field_assign)) e
    | EProj       e -> unreg print_projection e
    | EUpdate     e -> unreg print_update e
    | EVar        v -> [unreg lex v]
    | ECall       e -> unreg print_call e
    | EBytes      e -> [print_bytes e]
    | EUnit       _ -> print_unit
    | ETuple      e -> unreg print_tuple e
    | EPar        e -> [par [unreg (fun x -> print_expr x.inside) e]]
    | ELetIn      e -> unreg print_let_in e
    | EFun        e -> unreg print_fun e
    | ESeq        e -> unreg (print_injection print_expr) e
    | ECodeInj    e -> unreg print_code_inj e in
  Ast_node (NExpr,ast_x)

and print_code_inj {language; code; _} =
  [K.code_inj; unreg (unreg lex) language; print_expr code; K.rbracket]

and print_call (lambda,args) =
  print_expr lambda::List.map print_expr (Utils.nseq_to_list args)

and print_projection {struct_name; field_path; _} =
  let fields = print_nsepseq (const K.dot) print_selection field_path in
  unreg lex struct_name :: K.dot :: fields

and print_selection = function
  FieldName v   -> unreg lex v
  | Component cmp -> cmp.value |> snd |> Z.to_string |> lex

and print_update {record; updates; _} =
  let updates = unreg (print_ne_injection (unreg print_field_path_assign)) updates in
  let record  = print_path record in
  record :: K.kwd_with :: updates

and print_path x =
  let xs = match x with
    Name v -> [unreg lex v]
    | Path p -> unreg print_projection p in
  Ast_node (Path,xs)

and print_field_path_assign  {field_path; field_expr; _} =
  let path = print_path field_path in
  node Field [path; K.equal; print_expr field_expr]

and get_compund = function
  BeginEnd (_, _) -> (K.kwd_begin,K.kwd_end)
| Braces (_, _)   -> (K.lbrace,K.rbrace)
| Brackets (_, _) -> (K.lbracket,K.rbracket)

and print_injection : 'a. ('a -> ast) -> ('a injection) -> ast list =
  fun f {compound; elements; _} ->
  let elements = print_sepseq (const K.semi) f elements in
  match Option.map get_compund compound with
    None -> elements
  | Some (opening, closing) ->
     opening :: elements @ [closing]

and print_ne_injection : 'a. ('a -> ast) -> ('a ne_injection) -> ast list =
  fun f {compound; ne_elements; _} ->
  let elements = print_nsepseq (const K.semi) f ne_elements in
  match Option.map get_compund compound with
    None -> elements
  | Some (opening, closing) ->
     opening :: elements @ [closing]

and print_field_assign {field_name; field_expr; _} =
  node Field_assign [unreg lex field_name; K.equal; print_expr field_expr]

and print_let_in {binding; kwd_rec; body; attributes; _} =
  print_let_decl (kwd_rec,binding, attributes)::
    [K.kwd_in; print_expr body]

and print_fun {binders; lhs_type; body; _} =
  [K.kwd_fun]
  @ print_binders binders
  @ (opt_to_list (Option.map (fun (_,x) -> [K.colon; print_type_expr x]) lhs_type))
  @ [K.arrow; print_expr body]

and print_list = function
    ECons e -> unreg (print_bin_op K.cons) e
  | EListComp e -> unreg (print_injection print_expr) e

and print_constr_expr = function
  ENone      _ -> [K.c_None]
| ESomeApp   a -> [K.c_Some; unreg (fun (_,x) -> print_expr x) a]
| EConstrApp a ->
   unreg (fun (constr,arg) -> [unreg lex constr] @ Option.to_list (Option.map print_expr arg)) a

and print_tuple (head,tail) =
  if tail = []
  then [print_expr head]
  else print_nsepseq (const K.comma) print_expr (head,tail)

and print_string = function
  | Cat e -> unreg (print_bin_op K.cat) e
  | String e -> [K.dquote; lex e.value; K.dquote]
  | Verbatim e -> [K.lverbat; lex e.value; K.rverbat]

and print_annot x =
  [print_par (fun (expr,_, type_expr) -> [print_expr expr; K.colon; print_type_expr type_expr]) x]

and print_logic_expr = function
    BoolExpr e -> print_bool_expr e
  | CompExpr e -> print_comp_expr e

and print_bin_op op {arg1;arg2;_} =
  [print_expr arg1; op; print_expr arg2]

and print_un_op op {arg;_} =
  [op; print_expr arg]

and print_bool_expr = function
    Or   e  -> unreg (print_bin_op K.bool_or) e
  | And  e  -> unreg (print_bin_op K.bool_and) e
  | Not  e  -> unreg (print_un_op K.kwd_not) e
  | True  _ -> [K.kwd_true]
  | False _ -> [K.kwd_false]

and print_comp_expr = function
  | Lt    e -> unreg (print_bin_op K.lt)  e
  | Leq   e -> unreg (print_bin_op K.leq) e
  | Gt    e -> unreg (print_bin_op K.gt)  e
  | Geq   e -> unreg (print_bin_op K.geq) e
  | Equal e -> unreg (print_bin_op K.equal)  e
  | Neq   e -> unreg (print_bin_op K.neq) e

and print_arith_expr = function
    Add   e -> unreg (print_bin_op K.plus) e
  | Sub   e -> unreg (print_bin_op K.minus) e
  | Mult  e -> unreg (print_bin_op K.times) e
  | Div   e -> unreg (print_bin_op K.slash) e
  | Mod   e -> unreg (print_bin_op K.kwd_mod) e
  | Neg   e -> unreg (print_un_op K.minus) e
  | Int   e -> [unreg print_int e]
  | Nat   e -> [unreg print_nat e]
  | Mutez e -> [unreg print_mutez e]

and print_cond {test; ifso; ifnot; _} =
  [K.kwd_if; print_expr test; K.kwd_then; print_expr ifso]
  @ opt_to_list (Option.map (fun (_,x) -> [K.kwd_else; print_expr x]) ifnot)

and print_case_expr {expr; cases; _} =
  [K.kwd_match ; print_expr expr; K.kwd_with]
  @ unreg print_cases cases

and print_cases xs = print_nsepseq (const K.vbar) (unreg print_clause) xs

and print_clause {pattern; rhs; _} =
  node Clause [print_pattern pattern; K.arrow; print_expr rhs]

and print_type_expr : type_expr -> ast = function
  | TProd cartesian ->
     Ast_node (TCartesian,unreg (print_nsepseq (const K.times) print_type_expr) cartesian)
  | TSum sum ->
     Ast_node (TSum,unreg (print_nsepseq (const K.vbar) (fun x -> node TVariant (unreg print_variant x))) sum)
  | TRecord record ->
     node TRecord (unreg print_fields record)
  | TApp app ->
     node TApp (unreg print_type_app app)
  | TFun func ->
     unreg (fun (t1,_arrow,t2) -> Ast_node (TArrow, [print_type_expr t1; K.arrow; print_type_expr t2])) func
  | TPar te ->
     unreg (fun p -> par [print_type_expr p.inside]) te
  | TVar v ->
     unreg lex v
  | TWild _ ->
     lex "_"
  | TString lexe ->
     unreg lex lexe

and print_fields fields = print_ne_injection (unreg print_field_decl) fields

and print_field_decl {field_name; field_type; _} =
  node Field_decl [unreg lex field_name; K.colon; print_type_expr field_type]

and print_type_app (ctor, tuple) =
  [unreg print_type_tuple tuple ; unreg lex ctor]

and print_type_tuple {inside; _} =
  let xs = print_nsepseq (const K.comma) print_type_expr inside in
  let nxs = node TTuple xs in
  if List.length xs = 1
  then nxs
  else par [nxs]

and print_variant {constr; arg} =
  let constr = unreg lex constr in
  match arg with
    None -> [constr]
  | Some (_, e) ->
     [constr; K.kwd_of;print_type_expr e]

let print_type_decl {name; type_expr; _} =
  node NTypeDecl @@
    [K.kwd_type; unreg lex name; K.equal; print_type_expr type_expr]

let declaration = function
  | Let xs -> unreg (fun (_,a,b,c) -> print_let_decl (a,b,c)) xs
  | TypeDecl xs -> print_type_decl xs.value

let unparse_cst : Cameligo.CST.t -> node Pattern.ast =
  fun cst -> Ast_node (Ndeclarations, List.map declaration (Utils.nseq_to_list cst.decl))
