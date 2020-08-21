open Simple_utils.Region
open Cameligo.CST
open Pattern

type node =
  | Declarations
  | LetDecl | TypeDecl
  | Expr
  | Par
  | Attribute
  | Type
  | Pattern
  | Field | Field_assign | Field_decl
  | Path | Clause
  | Name

let string_of_node = function
  | Declarations -> "declarations"
  | LetDecl -> "letdecl"
  | TypeDecl -> "typedecl"
  | Expr -> "expr"
  | Par -> "par"
  | Attribute -> "attribute"
  | Type -> "type"
  | Pattern -> "pattern"
  | Field -> "field"
  | Field_assign -> "field_assign"
  | Field_decl -> "field_decl"
  | Path -> "path"
  | Clause -> "clause"
  | Name -> "name"

let node_of_string = function
  | "declarations" -> Some Declarations
  | "letdecl" -> Some LetDecl
  | "typedecl" -> Some TypeDecl
  | "expr" -> Some Expr
  | "par" -> Some Par
  | "attribute" -> Some Attribute
  | "type" -> Some Type
  | "pattern" -> Some Pattern
  | "field" -> Some Field
  | "field_assign" -> Some Field_assign
  | "field_decl" -> Some Field_decl
  | "path" -> Some Path
  | "clause" -> Some Clause
  | "name" -> Some Name
  | _ -> None

let node_of_string' x =
  match node_of_string x with
  | Some x -> x
  | None -> failwith "node_of_string"

type nonrec ast = node ast

let node t xs pos = Ast_node (pos, t, xs)

let lex x = node Name [Ast_lex x]

let rlex x = lex x.value x.region

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

let print_par f x =
  let {lpar;inside;rpar} = x.value in
  Ast_node (x.region, Par, K.lpar lpar :: f inside @ [K.rpar rpar])

let opt_to_list f x = Option.fold ~none:[] ~some:(fun x -> f x) x

let print_nsepseq f g (x,xs) =
  g x :: List.(concat (map (fun (x,y) -> [f x; g y]) xs))

let print_sepseq f g xs =
  match xs with
  | None -> []
  | Some xs -> print_nsepseq f g xs

let print_attribute x =
  node Attribute ([lex "[@@" x.region; rlex x; lex "]" x.region]) x.region
let print_attributes xs = List.map print_attribute xs

let print_int x =
  let (_,value) = x.value in
  lex (Z.to_string value) x.region

let print_nat x =
  let (_,value) = x.value in
  lex (Z.to_string value ^ "n") x.region

let print_mutez x =
  let (_,value) = x.value in
  lex (Z.to_string value ^ "mutez") x.region

let print_unit (lpar,rpar) = [K.lpar lpar; K.rpar rpar]

let rec print_let_decl x =
  let (kwd_let, kwd_rec, let_binding, attributes) = x.value in
  let xs =
    [K.kwd_let kwd_let]
    @ List.map K.kwd_rec (Option.to_list kwd_rec)
    @ print_let_binding x.region let_binding
    @ print_attributes attributes in
  node LetDecl xs x.region


and print_pattern pos x =
  let xs = match x with
      PConstr   p -> print_pconstr p
    | PUnit     u -> unreg print_unit u
    | PVar      v -> [rlex v]
    | PInt      i -> [print_int i]
    | PNat      n -> [print_nat n]
    | PBytes    b -> [print_bytes b]
    | PString   s -> print_string (String s)
    | PVerbatim s -> print_string (Verbatim s)
    | PWild     w -> [K.wild w]
    | PList     l -> print_plist l
    | PTuple    t -> print_ptuple t
    | PPar      p -> [print_par (fun x -> [print_pattern p.region x]) p]
    | PRecord   r -> unreg print_precord r
    | PTyped    t -> print_ptyped t in
  node Pattern xs pos

and print_pconstr = function
    PNone      n -> [K.c_None n]
  | PSomeApp   p ->
     let (s,x) = p.value in
     [K.c_Some s; print_pattern p.region x]
  | PFalse    f -> [K.kwd_false f]
  | PTrue     t -> [K.kwd_true t]
  | PConstrApp a -> print_pconstr_app a

and print_pconstr_app x = match x.value with
  | constr, None -> [rlex constr]
  | constr, Some pat -> [rlex constr; print_pattern x.region pat]

and print_ptuple x =
  print_nsepseq K.comma (print_pattern x.region) x.value

and print_plist = function
  PListComp cmp -> print_list_comp cmp
| PCons cons -> print_pcons cons

and print_list_comp e = print_injection (print_pattern e.region) e.value

and print_precord fields = print_ne_injection (print_field_pattern) fields

and print_field_pattern x =
  let {field_name; pattern; eq} = x.value in
  node Field [rlex field_name; K.equal eq; print_pattern x.region pattern] x.region

and print_pcons x =
  let (patt1, cons, patt2)  = x.value in
  [print_pattern x.region patt1; K.cons cons; print_pattern x.region patt2]

and print_ptyped x =
  let {pattern; type_expr; colon} = x.value in
  [print_pattern x.region pattern; K.colon colon; print_type_expr x.region type_expr]

and print_binders loc xs = List.map (print_pattern loc) (Utils.nseq_to_list xs)

and print_bytes e = lex ("0x" ^ Hex.show (snd e.value)) e.region

and print_let_binding loc {binders;lhs_type;let_rhs;eq} =
  print_binders loc binders
  @ (opt_to_list (fun (c,l) -> [K.colon c; print_type_expr loc l]) lhs_type)
  @ [K.equal eq; print_expr loc let_rhs]

and print_expr loc x =
  let ast_x : ast list =
    match x with
    | ECase       e -> print_case_expr e
    | ECond       e -> print_cond e
    | EAnnot      e -> print_annot e
    | ELogic      e -> print_logic_expr e
    | EArith      e -> print_arith_expr e
    | EString     e -> print_string e
    | EList       e -> print_list e
    | EConstr     e -> print_constr_expr e
    | ERecord     e -> unreg (print_ne_injection print_field_assign) e
    | EProj       e -> print_projection e
    | EUpdate     e -> print_update e
    | EVar        v -> [rlex v]
    | ECall       e -> print_call e
    | EBytes      e -> [print_bytes e]
    | EUnit       u -> unreg print_unit u
    | ETuple      e -> print_tuple e
    | EPar        e -> [print_par (fun x -> [print_expr e.region x]) e]
    | ELetIn      e -> print_let_in e
    | EFun        e -> print_fun e
    | ESeq        e -> print_injection (print_expr e.region) e.value
    | ECodeInj    e -> print_code_inj e in
  Ast_node (loc,Expr,ast_x)

and print_code_inj x =
  let {language; code; rbracket} = x.value in
  [K.code_inj x.region; unreg rlex language; print_expr x.region code; K.rbracket rbracket]

and print_call e =
  let loc = e.region in
  let (lambda,args) = e.value in
  print_expr loc lambda::List.map (print_expr loc) (Utils.nseq_to_list args)

and print_projection x =
  let {struct_name; field_path; selector} = x.value in
  let fields = print_nsepseq K.dot print_selection field_path in
  rlex struct_name :: K.dot selector :: fields

and print_selection = function
  FieldName v   -> rlex v
  | Component cmp ->
     lex (Z.to_string (snd cmp.value)) cmp.region

and print_update x =
  let {lbrace; record; kwd_with; updates; rbrace} = x.value in
  let updates = print_ne_injection (print_field_path_assign) updates.value in
  let record  = print_path x.region record in
  K.lbrace lbrace :: record :: K.kwd_with kwd_with :: updates @ [K.rbrace rbrace]

and print_path loc x =
  let xs = match x with
    Name v -> [rlex v]
    | Path p -> print_projection p in
  Ast_node (loc,Path,xs)

and print_field_path_assign x =
  let {field_path; field_expr; assignment} = x.value in
  let path = print_path x.region field_path in
  node Field [path; K.equal assignment; print_expr x.region field_expr] x.region

and get_compund = function
  BeginEnd (l,r) -> (K.kwd_begin l,K.kwd_end r)
| Braces (l,r)   -> (K.lbrace l,K.rbrace r)
| Brackets (l,r) -> (K.lbracket l,K.rbracket r)

and print_injection : 'a. ('a -> ast) -> ('a injection) -> ast list =
  fun f {compound; elements; terminator} ->
  let elements = print_sepseq K.semi f elements in
  match Option.map get_compund compound with
    None -> elements
  | Some (opening, closing) ->
     opening :: elements @ opt_to_list (fun x -> [K.semi x]) terminator @ [closing]

and print_ne_injection : 'a. ('a -> ast) -> ('a ne_injection) -> ast list =
  fun f {compound; ne_elements; terminator} ->
  let elements = print_nsepseq K.semi f ne_elements in
  match Option.map get_compund compound with
    None -> elements
  | Some (opening, closing) ->
     opening :: elements @ opt_to_list (fun x -> [K.semi x]) terminator @ [closing]

and print_field_assign x =
  let {field_name; field_expr; assignment} = x.value in
  node Field_assign [rlex field_name; K.equal assignment; print_expr x.region field_expr] x.region

and print_let_in x =
  let {kwd_let; binding; kwd_rec; body; kwd_in; attributes} = x.value in
  let value = (kwd_let, kwd_rec, binding, attributes) and region = x.region in
  print_let_decl {value;region} ::
    [K.kwd_in kwd_in; print_expr x.region body]

and print_fun x =
  let {kwd_fun; binders; lhs_type; arrow; body} = x.value in
  [K.kwd_fun kwd_fun]
  @ print_binders x.region binders
  @ (opt_to_list (fun (c,r) -> [K.colon c; print_type_expr x.region r]) lhs_type)
  @ [K.arrow arrow; print_expr x.region body]

and print_list = function
    ECons e -> print_bin_op K.cons e
  | EListComp e -> print_injection (print_expr e.region) e.value

and print_constr_expr = function
  | ENone      n -> [K.c_None n]
  | ESomeApp   x ->
     let (s,a) = x.value in
     [K.c_Some s; print_expr x.region a]
  | EConstrApp a ->
     let (constr,arg) = a.value in
     rlex constr :: Option.to_list (Option.map (print_expr a.region) arg)

and print_tuple x =
  let (head,tail) = x.value in
  if tail = []
  then [print_expr x.region head]
  else print_nsepseq K.comma (print_expr x.region) (head,tail)

and print_string = function
  | Cat e -> print_bin_op K.cat e
  | String e -> [K.dquote e.region; rlex e; K.dquote e.region]
  | Verbatim e -> [K.lverbat e.region; rlex e; K.rverbat e.region]

and print_annot x =
  [print_par (fun (expr, c, type_expr) ->
       [print_expr x.region expr; K.colon c; print_type_expr x.region type_expr]) x]

and print_logic_expr = function
    BoolExpr e -> print_bool_expr e
  | CompExpr e -> print_comp_expr e

and print_bin_op fop x =
  let {arg1;arg2; op} = x.value in
  [print_expr x.region arg1; fop op; print_expr x.region arg2]

and print_un_op fop x =
  let {arg;op} = x.value in
  [fop op; print_expr x.region arg]

and print_bool_expr = function
    Or   e  -> print_bin_op K.bool_or e
  | And  e  -> print_bin_op K.bool_and e
  | Not  e  -> print_un_op K.kwd_not e
  | True  t -> [K.kwd_true t]
  | False f -> [K.kwd_false f]

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
  | Int   e -> [print_int e]
  | Nat   e -> [print_nat e]
  | Mutez e -> [print_mutez e]

and print_cond x =
  let {kwd_if; test; kwd_then; ifso; ifnot} = x.value in
  [K.kwd_if kwd_if; print_expr x.region test; K.kwd_then kwd_then; print_expr x.region ifso]
  @ opt_to_list (fun (e,i) -> [K.kwd_else e; print_expr x.region i]) ifnot

and print_case_expr x =
  let {kwd_match; expr; kwd_with; lead_vbar; cases} = x.value in
  [K.kwd_match kwd_match; print_expr x.region expr; K.kwd_with kwd_with]
  @ List.map K.vbar (Option.to_list lead_vbar)
  @ unreg print_cases cases

and print_cases xs = print_nsepseq K.vbar print_clause xs

and print_clause x =
  let {pattern; arrow; rhs} = x.value in
  node Clause [print_pattern x.region pattern; K.arrow arrow; print_expr x.region rhs] x.region

and print_type_expr loc x =
  let xs = match x with
  | TProd cartesian ->
     print_nsepseq K.times (print_type_expr cartesian.region) cartesian.value
  | TSum sum ->
     print_nsepseq K.vbar print_variant sum.value
  | TRecord record ->
     unreg print_fields record
  | TApp app ->
     unreg print_type_app app
  | TFun x ->
     let (t1,arrow,t2) = x.value in
     [print_type_expr x.region t1; K.arrow arrow; print_type_expr x.region t2]
  | TPar te ->
     [print_par (fun p -> [print_type_expr te.region p]) te]
  | TVar v | TString v ->
     [rlex v]
  | TWild w ->
     [K.wild w]
  in node Type xs loc

and print_fields fields = print_ne_injection print_field_decl fields

and print_field_decl x =
  let {field_name; colon; field_type} = x.value in
  let xs = [rlex field_name; K.colon colon; print_type_expr x.region field_type] in
  node Field_decl xs x.region

and print_type_app (ctor, tuple) =
  [print_type_tuple tuple ; rlex ctor]

and print_type_tuple x =
  let xs = print_nsepseq K.comma (print_type_expr x.region) x.value.inside in
  let nxs = node Type xs x.region in
  if List.length xs = 1
  then nxs
  else print_par (fun _ -> [nxs]) x

and print_variant x =
  let  {constr; arg} = x.value in
  let constr = rlex constr in
  let xs = match arg with
    | None -> [constr]
    | Some (kwd_of, e) ->
       [constr; K.kwd_of kwd_of;print_type_expr x.region e]
  in node Type xs x.region

let print_type_decl x =
  let {kwd_type; name; eq; type_expr} = x.value in
  let xs = [K.kwd_type kwd_type; rlex name; K.equal eq; print_type_expr x.region type_expr] in
  node TypeDecl xs x.region


let declaration = function
  | Let xs -> print_let_decl xs
  | TypeDecl xs -> print_type_decl xs

let unparse_cst : Cameligo.CST.t -> node Pattern.ast list =
  fun cst -> List.map declaration (Utils.nseq_to_list cst.decl)
