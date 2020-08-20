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

type 'a ast =
  | Ast_lex of string
  | Ast_node of 'a * 'a ast list

let string_of_ast f x =
  let rec aux = function
    | Ast_lex x -> x
    | Ast_node (t,xs) -> f t ^ "%(" ^ String.concat " " (List.map aux xs) ^ "%)"
  in aux x

exception Failure

module SMap = Map.Make(String)

let add m k v =
  match SMap.find_opt k m with
  | None ->
     SMap.add k v m
  | Some w ->
     if v = w then m else raise Failure

let print_if b x =
  if b then print_endline x

let mat ?(debug=false) p f =
  let rec mat p f = match p,f with
    | [],[] -> (* END *)
       print_if debug "END";
       SMap.empty
    | Pat_lex l1::p, Ast_lex l2::f when l1 = l2 -> (* ELIM *)
       print_if debug "ELIM";
       mat p f
    | Pat_var(x,None)::Pat_lex l1::p, Ast_node(c,f1)::Ast_lex l2::f2 when l1 = l2 -> (* BIND1 *)
       print_if debug "BIND1";
       add (mat p f2) x (Ast_node(c,f1))
    | (Pat_var(x,Some c1)::Pat_lex l1::p, Ast_node(c2,f1)::Ast_lex(l2)::f2)
         when l1 = l2 && c1 = c2 -> (* BIND1 typed *)
       print_if debug "BIND1T";
       add (mat p f2) x (Ast_node(c2,f1))
    | (Pat_var(x,None)::p, Ast_node(c,f1)::(Ast_node(_,_)::_ as f2)) -> (* BIND2 *)
       print_if debug "BIND2";
       add (mat p f2) x (Ast_node(c,f1))
    | (Pat_var(x,Some c)::p, Ast_node(c1,f1)::(Ast_node(_,_)::_ as f2)) when c = c1 -> (* BIND2 typed *)
       print_if debug "BIND1T";
       add (mat p f2) x (Ast_node(c1,f1))
    | ([Pat_var(x,None)], [Ast_node(c,f)]) -> (* BIND3 *)
       print_if debug "BIND3";
       SMap.singleton x (Ast_node(c,f))
    | ([Pat_var(x,Some c1)], [Ast_node(c,f)]) when c1 = c -> (* BIND3 typed *)
       print_if debug "BIND3T";
       SMap.singleton x (Ast_node(c,f))
    | (Pat_pat p1::p2, Ast_node(_,f1)::f2) -> (* UNPAR1 *)
       print_if debug "UNPAR1";
       SMap.union (fun _ v1 v2 -> if v1=v2 then Some v1 else raise Failure) (mat p1 f1) (mat p2 f2)
    | (p,Ast_node(_,f1)::f2) -> (* UNPAR2 *)
       print_if debug "UNPAR2";
       mat p (f1 @ f2)
    | _ ->
       print_if debug "FAIL";
       raise Failure
  in mat p f

let pat_match ?(debug=false) p f =
  try ignore (mat ~debug [p] [f]); true
  with Failure -> false
