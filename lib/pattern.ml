open Unparser.Ast

(* This pattern matching algorithm is based on:
   Rinderknecht, Christian & Volanschi, Nic. (2010).
   Theory and practice of unparsed patterns for metacompilation. Science of Computer Programming. 75. 85-105.
*)

type 'a pattern =
  | Pat_lex of string
  | Pat_pat of 'a pattern list
  | Pat_var of string * 'a option

let string_of_pattern x =
  let rec aux = function
    | Pat_lex s -> s
    | Pat_pat xs -> "%(" ^ String.concat " " (List.map aux xs) ^ "%)"
    | Pat_var (s,_) -> s
  in aux x

let map_type_pattern f x =
  let rec aux = function
    | Pat_lex x -> Pat_lex x
    | Pat_pat xs -> Pat_pat (List.map aux xs)
    | Pat_var (s,x) -> Pat_var (s, Option.map f x) in
  aux x

exception Failure

module SMap = Map.Make(String)

let print_if b x =
  if b then print_endline x

let add ~debug eq_ast m k v =
  match SMap.find_opt k m with
  | None ->
     SMap.add k v m
  | Some w ->
     if eq_ast v w
     then m
     else (print_if debug "Fail during an add"; raise Failure)

let mat ?(debug=false) eq_ast p f =
  let rec mat p f = match p,f with
    | [],[] -> (* END *)
       print_if debug "END";
       SMap.empty
    | Pat_lex l1::p, Ast_lex l2::f when l1 = l2 -> (* ELIM *)
       print_if debug "ELIM";
       mat p f
    | Pat_var(x,None)::Pat_lex l1::p, (Ast_node _ as a1)::Ast_lex l2::f2 when l1 = l2 -> (* BIND1 *)
       print_if debug "BIND1";
       add ~debug eq_ast (mat p f2) x a1
    | (Pat_var(x,Some c1)::Pat_lex l1::p, (Ast_node(_,c2,_) as a1)::Ast_lex(l2)::f2)
         when l1 = l2 && c1 = c2 -> (* BIND1 typed *)
       print_if debug "BIND1T";
       add ~debug eq_ast (mat p f2) x a1
    | (Pat_var(x,None)::p, (Ast_node _ as a1) ::(Ast_node(_,_,_)::_ as f2)) -> (* BIND2 *)
       print_if debug "BIND2";
       add ~debug eq_ast (mat p f2) x a1
    | (Pat_var(x,Some c)::p, (Ast_node(_,c1,_) as a1)::(Ast_node(_,_,_)::_ as f2)) when c = c1 -> (* BIND2 typed *)
       print_if debug "BIND1T";
       add ~debug eq_ast (mat p f2) x a1
    | ([Pat_var(x,None)], [Ast_node _ as a1]) -> (* BIND3 *)
       print_if debug "BIND3";
       SMap.singleton x a1
    | ([Pat_var(x,Some c1)], [Ast_node(_,c,_) as a1]) when c1 = c -> (* BIND3 typed *)
       print_if debug "BIND3T";
       SMap.singleton x a1
    | (Pat_pat p1::p2, Ast_node(_,_,f1)::f2) -> (* UNPAR1 *)
       print_if debug "UNPAR1";
       SMap.union
         (fun _ v1 v2 ->
           if eq_ast v1 v2 then Some v1 else (print_if debug "Fail during UNPAR1"; raise Failure))
         (mat p1 f1) (mat p2 f2)
    | (p,Ast_node(_,_,f1)::f2) -> (* UNPAR2 *)
       print_if debug "UNPAR2";
       mat p (f1 @ f2)
    | _ ->
       print_if debug "FAIL";
       raise Failure
  in mat p f

let get_some xs =
  List.fold_left (fun acc x -> match acc with Some _ -> acc | None -> x) None xs

(* We run the pattern-matching algorithm on each node with the right type. *)
let pat_match ?(debug=false) pat typ ast =
  print_if debug
    ("PAT: " ^ string_of_pattern pat);
  let is_mat ast =
    try ignore (mat ~debug eq_ast [pat] [ast]);
        match ast with
        | Ast_node (loc,_,_) -> Some (Simple_utils.Location.File loc)
        | Ast_lex _ -> Some Simple_utils.Location.generated
    with Failure -> None in
  let lex _ = None in
  let node reg typ' xs thunk =
    if typ=typ'
    then begin
        print_if debug ("AST: " ^ string_of_ast (Ast_node (reg,typ,xs)));
        match is_mat (Ast_node (reg,typ,xs)) with
        | Some _ as x -> x
        | None -> get_some (thunk ()) end
    else get_some (thunk ()) in
  thunked_fold_ast lex node ast
