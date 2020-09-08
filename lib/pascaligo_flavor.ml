open Pascaligo.CST
open Simple_utils.Region

type flavor = Terse | Verbose

let string_of_flavor = function
  | Terse -> "Terse"
  | Verbose -> "Verbose"

type wrong_flavor =
  { actual   : flavor;
    expected : string;
    got      : string;
    reg      : region
  }

exception WrongFlavor of wrong_flavor

let flavor_terminator reg flavor option =
  match flavor,option with
  | None,None -> Some Verbose
  | None,Some _ -> Some Terse
  | Some Terse, Some _ | Some Verbose, None -> flavor
  | Some Terse, None ->
     raise (WrongFlavor {actual=Terse; expected="semicolon"; got="nothing"; reg})
  | Some Verbose, Some reg ->
     raise (WrongFlavor {actual=Verbose; expected="nothing"; got="semicolon"; reg})

let favor_lead reg flavor option =
  match flavor,option with
  | None,None -> Some Verbose
  | None,Some _ -> Some Terse
  | Some Terse, Some _ | Some Verbose, None -> flavor
  | Some Terse, None ->
     raise (WrongFlavor {actual=Terse; expected="vertical bar"; got="nothing"; reg})
  | Some Verbose, Some reg ->
     raise (WrongFlavor {actual=Verbose; expected="nothing"; got="vertical bar"; reg})

let flavor_block_enclosing flavor enclos =
  match flavor,enclos with
  | None,Block _ -> Some Terse
  | None,BeginEnd _ -> Some Verbose
  | Some Terse,Block _ | Some Verbose, BeginEnd _ -> flavor
  | Some Verbose,Block (reg,_,_) ->
     raise (WrongFlavor {actual=Verbose; expected="begin"; got="block"; reg})
  | Some Terse, BeginEnd (reg,_) ->
     raise (WrongFlavor {actual=Terse; expected="block"; got="begin"; reg})

let flavor_enclosing flavor enclosing =
  match flavor,enclosing with
  | None, Brackets _ -> Some Terse
  | None, End _      -> Some Verbose
  | Some Terse,Brackets _ | Some Verbose, End _ -> flavor
  | Some Verbose,Brackets (_,reg) ->
     raise (WrongFlavor {actual=Verbose; expected="end"; got="right bracket"; reg})
  | Some Terse, End reg ->
     raise (WrongFlavor {actual=Terse; expected="right bracket"; got="end"; reg})

let check_opt flavor f x =
  Option.fold ~none:flavor ~some:(f flavor) x

let check_ne_injection region flavor f {enclosing;ne_elements;terminator;_} =
  let flavor = flavor_terminator region flavor terminator in
  let flavor = flavor_enclosing flavor enclosing in
  List.fold_left f flavor (Utils.nsepseq_to_list ne_elements)

let check_injection region flavor f {enclosing;elements;terminator;_} =
  let flavor = flavor_terminator region flavor terminator in
  let flavor = flavor_enclosing flavor enclosing in
  List.fold_left f flavor (Utils.sepseq_to_list elements)

let rec check_type_expr flavor = function
  | TVar _ | TWild _ | TString _ ->
     flavor
  | TProd   x ->
     List.fold_left check_type_expr flavor (Utils.nsepseq_to_list x.value)
  | TSum    x -> (* TODO LEAD VBAR ? *)
     List.fold_left check_tvariant flavor (Utils.nsepseq_to_list x.value)
  | TRecord x ->
     check_trecord flavor x
  | TApp    {value=(_,x);_} ->
     List.fold_left check_type_expr flavor (Utils.nsepseq_to_list x.value.inside)
  | TFun    {value=(l,_,r);_} ->
     check_type_expr (check_type_expr flavor l)  r
  | TPar    x ->
     check_type_expr flavor x.value.inside

and check_tvariant flavor {value;_} =
  check_opt flavor (fun flavor (_,x) -> check_type_expr flavor x) value.arg

and check_trecord flavor x =
  check_ne_injection x.region
    flavor (fun flavor x -> check_type_expr flavor x.value.field_type) x.value

let check_typedecl flavor ({terminator;type_expr;_} : type_decl) =
  let flavor = flavor_terminator (type_expr_to_region type_expr) flavor terminator in
  check_type_expr flavor type_expr

let rec check_expr (flavor : flavor option) = function
  | EVar _ | EUnit _ | EString _ | EBytes _ | EProj _ ->
     flavor
  | ECase    x ->
     check_case_expr x.region check_expr flavor x.value
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
     check_record x.region flavor x.value
  | EUpdate  x ->
     check_update x.region flavor x.value
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

and check_update region flavor {updates;_} =
  check_ne_injection region flavor
    (fun flavor (x:field_path_assignment reg) -> check_expr flavor x.value.field_expr) updates.value

and check_record region flavor x =
  check_ne_injection region flavor
    (fun flavor (x:field_assignment reg) -> check_expr flavor x.value.field_expr) x

and check_case_expr :
      'a. region -> (flavor option -> 'a -> flavor option) -> flavor option -> ('a case) -> flavor option =
  fun region f flavor {expr; enclosing; cases; lead_vbar; _} -> (* TODO LEADVBAR *)
  let flavor = check_expr flavor expr in
  let flavor = flavor_enclosing flavor enclosing in
  let flavor = favor_lead region flavor lead_vbar in
  List.fold_left (check_case_clause f) flavor (Utils.nsepseq_to_list cases.value)

and check_case_clause :
      'a. (flavor option -> 'a -> flavor option) -> flavor option -> 'a case_clause reg -> flavor option =
  fun f flavor {value;_} ->
  let flavor = check_pattern flavor value.pattern in
  f flavor value.rhs

and check_pattern flavor : pattern -> flavor option = function
  | PVar _ | PWild _ | PInt _ | PBytes _ | PString _ | PNat _ ->
     flavor
  | PConstr x ->
     check_constr_pattern flavor x
  | PList   x ->
     check_list_pattern flavor x
  | PTuple  x ->
     check_tuple_pattern flavor x

and check_constr_pattern flavor = function
  | PUnit _ | PFalse _ | PTrue _ | PNone _ ->
     flavor
  | PSomeApp {value=(_,pat);_} ->
     check_pattern flavor pat.value.inside
  | PConstrApp {value=(_,tuple);_} ->
     check_opt flavor check_tuple_pattern tuple

and check_tuple_pattern flavor xs =
  List.fold_left check_pattern flavor (Utils.nsepseq_to_list xs.value.inside)

and check_list_pattern flavor = function
  | PNil _ ->
     flavor
  | PListComp x ->
     check_injection x.region flavor check_pattern x.value
  | PParCons  x ->
     let (l,_,r) = x.value.inside in
     check_pattern (check_pattern flavor l) r;
  | PCons x ->
     List.fold_left check_pattern flavor (Utils.nsepseq_to_list x.value)

and check_list_expr flavor = function
  | ENil _ ->
     flavor
  | ECons x ->
     check_binop flavor x.value
  | EListComp x ->
     check_injection x.region flavor check_expr x.value

and check_constr_expr flavor = function
  | NoneExpr _ ->
     flavor
  | SomeApp {value;_} ->
     check_tuple flavor (snd value).value
  | ConstrApp {value;_} ->
    check_opt flavor (fun flavor (x : tuple_expr) -> check_tuple flavor x.value) (snd value)

and check_set_expr flavor = function
  | SetInj x ->
     check_injection x.region flavor check_expr x.value
  | SetMem x ->
     let {set;element;_} = x.value in
     let flavor = check_expr flavor set in
     check_expr flavor element

and check_cond_expr flavor {test;ifso;terminator;ifnot;kwd_else;_}=
  let flavor = check_expr flavor test in
  let flavor = check_expr flavor ifso in
  let flavor = flavor_terminator kwd_else flavor terminator in
  check_expr flavor ifnot

and check_if_clause flavor = function
  | ClauseInstr x ->
     check_instr flavor x
  | ClauseBlock x ->
     check_clause_block flavor x

and check_clause_block flavor = function
  | LongBlock x ->
     check_block flavor x
  | ShortBlock x ->
     let (i,terminator) = x.value.inside in
     let flavor = flavor_terminator x.region flavor terminator in
     check_statements flavor i

and check_block flavor x =
  let {enclosing; statements; terminator; _} = x.value in
  let flavor = flavor_terminator x.region flavor terminator in
  let flavor = flavor_block_enclosing flavor enclosing in
  check_statements flavor statements

and check_statements flavor (x:statements) =
  List.fold_left check_statement flavor (Utils.nsepseq_to_list x)

and check_attr_decl flavor x =
  check_ne_injection x.region flavor (fun flavor _ -> flavor) x.value

and check_statement flavor = function
  | Attr x -> (* TODO *)
     check_attr_decl flavor x
  | Instr x ->
     check_instr flavor x
  | Data x ->
     check_data_decl flavor x

and check_instr flavor = function
  | Skip _ ->
     flavor
  | Cond        x ->
     check_conditional flavor x.value
  | CaseInstr   x ->
     check_case_expr x.region check_if_clause flavor x.value
  | Assign      x ->
     check_assignment flavor x.value
  | Loop        x ->
     check_loop flavor x
  | ProcCall    x ->
     check_fun_call flavor x.value
  | RecordPatch x ->
     check_record_patch x.region flavor x.value
  | MapPatch    x ->
     check_map_patch x.region flavor x.value
  | SetPatch    x ->
     check_set_patch x.region flavor x.value
  | MapRemove   x ->
     check_map_remove flavor x.value
  | SetRemove   x ->
     check_set_remove flavor x.value

and check_map_patch region flavor {map_inj;_} =
  check_ne_injection region flavor check_binding map_inj.value

and check_set_patch region flavor {set_inj;_} =
  check_ne_injection region flavor check_expr set_inj.value

and check_map_remove flavor {key;_} =
  check_expr flavor key

and check_set_remove flavor {element;_} =
  check_expr flavor element

and check_record_patch region flavor {record_inj;_} =
  check_record region flavor record_inj.value

and check_loop flavor = function
  | While x ->
     check_while_loop flavor x.value
  | For x ->
     check_for_loop flavor x

and check_while_loop flavor {cond;block;_} =
  let flavor = check_expr flavor cond in
  check_block flavor block

and check_for_loop flavor = function
  | ForInt x ->
     check_for_int flavor x.value
  | ForCollect x ->
     check_for_collect flavor x.value

and check_for_int flavor {init;bound;step;block;_} =
  let flavor = check_expr flavor init in
  let flavor = check_expr flavor bound in
  let flavor = check_opt flavor (fun flavor (_,x) -> check_expr flavor x) step in
  check_block flavor block

and check_for_collect flavor {expr;block;_} =
  let flavor = check_expr flavor expr in
  check_block flavor block

and check_assignment flavor {lhs;rhs;_} =
  let flavor = check_lhs flavor lhs in
  check_expr flavor rhs

and check_lhs flavor = function
  | Path _ ->
     flavor
  | MapPath {value={index;_};_} ->
     check_expr flavor index.value.inside

and check_conditional flavor {test;ifso;terminator;ifnot;kwd_else;_}=
  let flavor = check_expr flavor test in
  let flavor = check_if_clause flavor ifso in
  let flavor = flavor_terminator kwd_else flavor terminator in
  check_if_clause flavor ifnot

and check_data_decl flavor = function
  | LocalConst x ->
     check_constdecl x.region flavor x.value
  | LocalVar x ->
     check_vardecl x.region flavor x.value
  | LocalFun x ->
     check_fundecl x.region flavor x.value

and check_vardecl region flavor {var_type;init;terminator;_} =
  let flavor = flavor_terminator region flavor terminator in
  let flavor = check_opt flavor (fun flavor (_,x) -> check_type_expr flavor x) var_type in
  check_expr flavor init

and check_map_expr flavor = function
  | MapLookUp {value;_} ->
     check_expr flavor value.index.value.inside
  | MapInj x | BigMapInj x ->
     check_injection x.region flavor check_binding x.value

and check_binding flavor {value={source;image;_};_} =
  let flavor = check_expr flavor source in
  check_expr flavor image

and check_fun_call flavor (expr,args) =
  let flavor = check_expr flavor expr in
  check_tuple flavor args.value

and check_tuple flavor x =
  List.fold_left check_expr flavor (Utils.nsepseq_to_list x.inside)

and check_fun_expr flavor {param;ret_type;return;_} =
  let flavor = check_parameters flavor param in
  let flavor = check_opt flavor (fun flavor (_,x) -> check_type_expr flavor x) ret_type in
  check_expr flavor return

and check_parameters flavor xs =
  List.fold_left check_param_decl flavor (Utils.nsepseq_to_list xs.value.inside)

and check_param_decl flavor = function
  | ParamConst {value={param_type;_};_} | ParamVar {value={param_type;_};_} ->
     check_opt flavor (fun flavor (_,x) -> check_type_expr flavor x) param_type

and check_block_with flavor {block;expr;_} =
  let flavor = check_block flavor block in
  check_expr flavor expr

and check_annot flavor x =
  let (e,_,t) = x.inside in
  let flavor = check_expr flavor e in
  check_type_expr flavor t

and check_logic_expr flavor = function
  | BoolExpr x -> check_bool_expr flavor x
  | CompExpr x -> check_comp_expr flavor x

and check_binop flavor {arg1;arg2;_} =
  let flavor = check_expr flavor arg1 in
  check_expr flavor arg2

and check_unop flavor {arg;_} =
  check_expr flavor arg

and check_bool_expr flavor = function
  | False _ | True _ ->
     flavor
  | Or x | And x ->
     check_binop flavor x.value
  | Not x ->
     check_unop flavor x.value

and check_comp_expr flavor = function
  | Lt x | Leq x | Gt x | Geq x | Equal x | Neq x ->
     check_binop flavor x.value

and check_arith_expr flavor = function
  | Int _ | Nat _ | Mutez _ ->
     flavor
  | Add x | Sub x | Mult x | Div x | Mod x ->
     check_binop flavor x.value
  | Neg x ->
     check_unop flavor x.value

and check_constdecl region flavor {const_type; init; terminator; attributes; _} =
  let flavor = check_opt flavor (fun flavor (_,x) -> check_type_expr flavor x) const_type in
  let flavor = check_opt flavor check_attr_decl attributes in
  let flavor = check_expr flavor init in
  flavor_terminator region flavor terminator

and check_fundecl region flavor {param;ret_type;return; terminator; attributes; _} =
  let flavor = flavor_terminator region flavor terminator in
  let flavor = check_parameters flavor param in
  let flavor = check_opt flavor check_attr_decl attributes in
  let flavor = check_opt flavor (fun flavor (_,x) -> check_type_expr flavor x) ret_type in
  check_expr flavor return

let check_declaration flavor = function
  | AttrDecl x ->
     check_attr_decl flavor x
  | TypeDecl x ->
     check_typedecl flavor x.value
  | ConstDecl x ->
     check_constdecl x.region flavor x.value
  | FunDecl x ->
     check_fundecl x.region flavor x.value

let check_program flavor xs =
  try
    ignore @@
      List.fold_left check_declaration flavor (Utils.nseq_to_list xs.decl);
    None
  with
  | WrongFlavor x -> Some x


let verify_program ?flavor p =
  match check_program flavor p with
  | None -> []
  | Some {actual;expected;got;reg} ->
     let str =
       "Mixed PascalLigo flavors. With inferred "
       ^ string_of_flavor actual
       ^ " flavor, expected "
       ^ expected
       ^ " but got "
       ^ got ^ "." in
     [Simple_utils.Location.File reg,str]
