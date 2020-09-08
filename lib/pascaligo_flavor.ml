open Pascaligo.CST
open Simple_utils.Region

type flavor = Terse | Verbose

exception WrongFlavor

let flavor_opt flavor option =
  match flavor,option with
  | None,None -> Some Terse
  | None,Some _ -> Some Verbose
  | Some Terse,None | Some Verbose, Some _ -> flavor
  | _ -> raise WrongFlavor

let flavor_block_enclosing flavor enclos =
  match flavor,enclos with
  | None,Block _ -> Some Terse
  | None,BeginEnd _ -> Some Verbose
  | Some Terse,Block _ | Some Verbose, BeginEnd _ -> flavor
  | _ -> raise WrongFlavor

let flavor_enclosing flavor enclosing =
  match flavor,enclosing with
  | None, Brackets _ -> Some Terse
  | None, End _      -> Some Verbose
  | Some Terse,Brackets _ | Some Verbose, End _ -> flavor
  | _ -> raise WrongFlavor

let check_opt f x =
  Option.fold ~none:() ~some:f x

let check_ne_injection flavor f {enclosing;ne_elements;terminator;_} =
  flavor_opt flavor terminator;
  flavor_enclosing flavor enclosing;
  List.iter f (Utils.nsepseq_to_list ne_elements)

let check_injection flavor f {enclosing;elements;terminator;_} =
  flavor_opt flavor terminator;
  flavor_enclosing flavor enclosing;
  List.iter f (Utils.sepseq_to_list elements)

let check_type_expr flavor =
  let rec aux = function
    | TVar _ | TWild _ | TString _ ->
       ()
    | TProd   x ->
       List.iter aux (Utils.nsepseq_to_list x.value)
    | TSum    x -> (* TODO LEAD VBAR ? *)
       List.iter variant (Utils.nsepseq_to_list x.value)
    | TRecord x ->
       record x.value
    | TApp    {value=(_,x);_} ->
       List.iter aux (Utils.nsepseq_to_list x.value.inside)
    | TFun    {value=(l,_,r);_} ->
       aux l; aux r
    | TPar    x ->
       aux x.value.inside
  and variant {value;_} =
    Option.fold ~none:() ~some:(fun (_,x) -> aux x) value.arg
  and record x =
    check_ne_injection flavor (fun {value;_} -> aux value.field_type) x
  in aux

let check_typedecl flavor ({terminator;type_expr;_} : type_decl) =
  flavor_opt flavor terminator;
  check_type_expr flavor type_expr

let rec check_expr flavor = function
  | EVar _ | EUnit _ | EString _ | EBytes _ | EProj _ -> ()
  | ECase    x ->
     check_case_expr flavor check_expr x.value
  | ECond    x ->
     check_cond_expr flavor x.value
  | EAnnot   x ->
     check_annot flavor x.value
  | ELogic   x ->
     check_logic_expr flavor x
  | EArith   x ->
     check_arith_expr flavor x
  | EList    x ->
     check_list_expr flavor x
  | ESet     x ->
     check_set_expr flavor x
  | EConstr  x ->
     check_constr_expr flavor x
  | ERecord  x ->
     check_record flavor x.value
  | EUpdate  x ->
     check_update flavor x.value
  | EMap     x ->
     check_map_expr flavor x
  | ECall    x ->
     check_fun_call flavor x.value
  | ETuple   x ->
     check_tuple flavor x.value
  | EPar     x ->
     check_expr flavor x.value.inside
  | EFun     x ->
     check_fun_expr flavor x.value
  | ECodeInj x ->
     check_expr flavor x.value.code
  | EBlock   x ->
     check_block_with flavor x.value

and check_update flavor {updates;_} =
  check_ne_injection flavor
    (fun (x:field_path_assignment reg) -> check_expr flavor x.value.field_expr) updates.value

and check_record flavor x =
  check_ne_injection flavor
    (fun (x:field_assignment reg) -> check_expr flavor x.value.field_expr) x

and check_case_expr : 'a. flavor -> (flavor -> 'a -> unit) -> ('a case) -> unit =
  fun flavor f {expr; enclosing; cases; _} -> (* TODO LEADVBAR *)
  check_expr flavor expr;
  flavor_enclosing flavor enclosing;
  List.iter (check_case_clause flavor f) (Utils.nsepseq_to_list cases.value)

and check_case_clause : 'a. flavor -> (flavor -> 'a -> unit) -> 'a case_clause reg -> unit =
  fun flavor f {value;_} ->
  check_pattern flavor value.pattern;
  f flavor value.rhs

and check_pattern flavor = function
  | PVar _ | PWild _ | PInt _ | PBytes _ | PString _ | PNat _ ->
     ()
  | PConstr x ->
     check_constr_pattern flavor x
  | PList   x ->
     check_list_pattern flavor x
  | PTuple  x ->
     check_tuple_pattern flavor x

and check_constr_pattern flavor = function
  | PUnit _ |PFalse _ | PTrue _ | PNone _ -> ()
  | PSomeApp {value=(_,pat);_} ->
     check_pattern flavor pat.value.inside
  | PConstrApp {value=(_,tuple);_} ->
     check_opt (check_tuple_pattern flavor) tuple

and check_tuple_pattern flavor xs =
  List.iter (check_pattern flavor) (Utils.nsepseq_to_list xs.value.inside)

and check_list_pattern flavor = function
  | PNil _ ->
     ()
  | PListComp x ->
     check_injection flavor (check_pattern flavor) x.value
  | PParCons  x ->
     let (l,_,r) = x.value.inside in
     check_pattern flavor l;
     check_pattern flavor r
  | PCons x ->
     List.iter (check_pattern flavor) (Utils.nsepseq_to_list x.value)

and check_list_expr flavor = function
  | ENil _ ->
     ()
  | ECons x ->
     check_binop flavor x.value
  | EListComp x ->
     check_injection flavor (check_expr flavor) x.value

and check_constr_expr flavor = function
  | NoneExpr _ ->
     ()
  | SomeApp {value;_} ->
     check_tuple flavor (snd value).value
  | ConstrApp {value;_} ->
    check_opt (fun (x : tuple_expr) -> check_tuple flavor x.value) (snd value)

and check_set_expr flavor = function
  | SetInj x ->
     check_injection flavor (check_expr flavor) x.value
  | SetMem x ->
     let {set;element;_} = x.value in
     check_expr flavor set;
     check_expr flavor element

and check_cond_expr flavor {test;ifso;terminator;ifnot;_}=
  check_expr flavor test;
  check_expr flavor ifso;
  flavor_opt flavor terminator;
  check_expr flavor ifnot

and check_if_clause flavor = function
  | ClauseInstr x ->
     check_instr flavor x
  | ClauseBlock x ->
     check_clause_block flavor x

and check_clause_block flavor = function
  | LongBlock x ->
     check_block flavor x.value
  | ShortBlock x ->
     let (x,terminator) = x.value.inside in
     flavor_opt flavor terminator;
     check_statements flavor x

and check_block flavor {enclosing; statements; terminator; _} =
  flavor_opt flavor terminator;
  flavor_block_enclosing flavor enclosing;
  check_statements flavor statements

and check_statements flavor (x:statements) =
  List.iter (check_statement flavor) (Utils.nsepseq_to_list x)

and check_statement flavor = function
  | Attr _ ->
     ()
  | Instr x ->
     check_instr flavor x
  | Data x ->
     check_data_decl flavor x

and check_instr flavor = function
  | Skip _ ->
     ()
  | Cond        x ->
     check_conditional flavor x.value
  | CaseInstr   x ->
     check_case_expr flavor check_if_clause x.value
  | Assign      x ->
     check_assignment flavor x.value
  | Loop        x ->
     check_loop flavor x
  | ProcCall    x ->
     check_fun_call flavor x.value
  | RecordPatch x ->
     check_record_patch flavor x.value
  | MapPatch    x ->
     check_map_patch flavor x.value
  | SetPatch    x ->
     check_set_patch flavor x.value
  | MapRemove   x ->
     check_map_remove flavor x.value
  | SetRemove   x ->
     check_set_remove flavor x.value

and check_map_patch flavor {map_inj;_} =
  check_ne_injection flavor (check_binding flavor) map_inj.value

and check_set_patch flavor {set_inj;_} =
  check_ne_injection flavor (check_expr flavor) set_inj.value

and check_map_remove flavor {key;_} =
  check_expr flavor key

and check_set_remove flavor {element;_} =
  check_expr flavor element

and check_record_patch flavor {record_inj;_} =
  check_record flavor record_inj.value

and check_loop flavor = function
  | While x ->
     check_while_loop flavor x.value
  | For x ->
     check_for_loop flavor x

and check_while_loop flavor {cond;block;_} =
  check_expr flavor cond;
  check_block flavor block.value

and check_for_loop flavor = function
  | ForInt x ->
     check_for_int flavor x.value
  | ForCollect x ->
     check_for_collect flavor x.value

and check_for_int flavor {init;bound;step;block;_} =
  check_expr flavor init;
  check_expr flavor bound;
  check_opt (fun (_,x) -> check_expr flavor x) step;
  check_block flavor block.value

and check_for_collect flavor {expr;block;_} =
  check_expr flavor expr;
  check_block flavor block.value

and check_assignment flavor {lhs;rhs;_} =
  check_lhs flavor lhs;
  check_expr flavor rhs

and check_lhs flavor = function
  | Path _ ->
     ()
  | MapPath {value={index;_};_} ->
     check_expr flavor index.value.inside

and check_conditional flavor {test;ifso;terminator;ifnot;_}=
  check_expr flavor test;
  check_if_clause flavor ifso;
  flavor_opt flavor terminator;
  check_if_clause flavor ifnot

and check_data_decl flavor = function
  | LocalConst x ->
     check_constdecl flavor x.value
  | LocalVar x ->
     check_vardecl flavor x.value
  | LocalFun x ->
     check_fundecl flavor x.value

and check_vardecl flavor {var_type;init;terminator;_} =
  flavor_opt flavor terminator;
  check_opt (fun (_,x) -> check_type_expr flavor x) var_type;
  check_expr flavor init

and check_map_expr flavor = function
  | MapLookUp {value;_} ->
     check_expr flavor value.index.value.inside
  | MapInj x | BigMapInj x ->
     check_injection flavor (check_binding flavor) x.value

and check_binding flavor {value={source;image;_};_} =
  check_expr flavor source;
  check_expr flavor image

and check_fun_call flavor (expr,args) =
  check_expr flavor expr;
  check_tuple flavor args.value

and check_tuple flavor x =
  List.iter (check_expr flavor) (Utils.nsepseq_to_list x.inside)

and check_fun_expr flavor {param;ret_type;return;_} =
  check_parameters flavor param;
  check_opt (fun (_,x) -> check_type_expr flavor x) ret_type;
  check_expr flavor return

and check_parameters flavor xs =
  List.iter (check_param_decl flavor) (Utils.nsepseq_to_list xs.value.inside)

and check_param_decl flavor = function
  | ParamConst {value={param_type;_};_} | ParamVar {value={param_type;_};_} ->
     check_opt (fun (_,x) -> check_type_expr flavor x) param_type

and check_block_with flavor {block;expr;_} =
  check_block flavor block.value;
  check_expr flavor expr

and check_annot flavor x =
  let (e,_,t) = x.inside in
  check_expr flavor e;
  check_type_expr flavor t

and check_logic_expr flavor = function
  | BoolExpr x -> check_bool_expr flavor x
  | CompExpr x -> check_comp_expr flavor x

and check_binop flavor {arg1;arg2;_} =
  check_expr flavor arg1;
  check_expr flavor arg2

and check_unop flavor {arg;_} =
  check_expr flavor arg

and check_bool_expr flavor = function
  | False _ | True _ ->
     ()
  | Or x | And x ->
     check_binop flavor x.value
  | Not x ->
     check_unop flavor x.value

and check_comp_expr flavor = function
  | Lt x | Leq x | Gt x | Geq x | Equal x | Neq x ->
     check_binop flavor x.value

and check_arith_expr flavor = function
  | Int _ | Nat _ | Mutez _ ->
     ()
  | Add x | Sub x | Mult x | Div x | Mod x ->
     check_binop flavor x.value
  | Neg x ->
     check_unop flavor x.value

and check_constdecl flavor {const_type; init; terminator;_} =
  check_opt (fun (_,x) -> check_type_expr flavor x) const_type;
  check_expr flavor init;
  flavor_opt flavor terminator

and check_fundecl flavor {param;ret_type;return; terminator; _} =
  flavor_opt flavor terminator;
  check_parameters flavor param;
  check_opt (fun (_,x) -> check_type_expr flavor x) ret_type;
  check_expr flavor return

let check_declaration flavor = function
  | AttrDecl _ ->
     ()
  | TypeDecl x ->
     check_typedecl flavor x.value
  | ConstDecl x ->
     check_constdecl flavor x.value
  | FunDecl x ->
     check_fundecl flavor x.value

let check_program flavor xs =
  try
    List.iter (check_declaration flavor) (Utils.nseq_to_list xs.decl);
    None
  with
  | WrongFlavor -> Some ()
