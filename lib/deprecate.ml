open Ast_imperative

(* This is just a traversal of Ast_imperative to catch
   the deprecated constants.
*)

let rec dep_expr acc x =
  match x.expression_content with
  | E_literal _ | E_variable _ | E_skip ->
     acc
  | E_constant {cons_name;arguments} ->
     let acc = match cons_name with
     | Deprecated e -> (x.location,e.name)::acc
     | Const _ -> acc in
     List.fold_left dep_expr acc arguments
  | E_application {lamb;args} ->
     dep_expr (dep_expr acc lamb) args
  | E_lambda {result;_} | E_recursive {lambda={result;_};_} ->
     dep_expr acc result
  | E_let_in {rhs;let_result;_} ->
     dep_expr (dep_expr acc rhs) let_result
  | E_raw_code {code;_} ->
     dep_expr acc code
  (* Variant *)
  | E_constructor {element;_} ->
     dep_expr acc element
  | E_matching {matchee;cases} ->
     dep_cases (dep_expr acc matchee) cases
  (* Record *)
  | E_record record ->
     LMap.fold (fun _ x acc -> dep_expr acc x) record acc
  | E_accessor {record;path;_} ->
     List.fold_left dep_access (dep_expr acc record) path
  | E_update  {record;update;path;_} ->
     let acc = dep_expr (dep_expr acc record) update in
     List.fold_left dep_access acc path
  (* Advanced *)
  | E_ascription {anno_expr;_} ->
     dep_expr acc anno_expr
  (* Sugar *)
  | E_cond {condition;then_clause;else_clause} ->
     List.fold_left dep_expr acc [condition;then_clause;else_clause]
  | E_sequence {expr1;expr2} ->
     dep_expr (dep_expr acc expr1) expr2
  | E_tuple xs | E_list xs | E_set xs ->
     List.fold_left dep_expr acc xs
  (* Data Structures *)
  | E_map xs | E_big_map xs ->
     List.fold_left (fun acc (x,y) -> dep_expr (dep_expr acc x) y) acc xs
  (* Imperative *)
  | E_assign {expression;access_path;_} ->
     List.fold_left dep_access (dep_expr acc expression) access_path
  | E_for {start;final;increment;body;_} ->
     List.fold_left dep_expr acc [start;final;increment;body]
  | E_for_each {collection;body;_} ->
     dep_expr (dep_expr acc collection) body
  | E_while {condition;body} ->
     dep_expr (dep_expr acc condition) body

and dep_cases acc = function
  | Match_variant xs ->
     List.fold_left (fun acc (_,x) -> dep_expr acc x) acc xs
  | Match_list {match_nil;match_cons=(_,_,x)} ->
     dep_expr (dep_expr acc match_nil) x
  | Match_option {match_none;match_some=(_,x)} ->
     dep_expr (dep_expr acc match_none) x
  | Match_tuple  (_,x) | Match_record (_,x) | Match_variable (_,x) ->
     dep_expr acc x

and dep_access acc = function
  | Access_tuple _ | Access_record _ -> acc
  | Access_map x -> dep_expr acc x

let dep_decl acc x =
  match Location.unwrap x with
  | Declaration_type _ -> acc
  | Declaration_constant (_,_,_,x) -> dep_expr acc x

let run xs =
  List.fold_left dep_decl [] xs

let format xs =
  List.map (fun (x,n) -> x, "Deprecated function " ^ n ^ ".") xs
