open Rules

(* Create the message for a depreciated function *)
let make_dep_msg {dep; dep_version; dep_replacement; dep_message} =
  let with_default f = Option.fold ~none:"" ~some:f in
  let repl = with_default (fun x -> "A possible replacement is " ^ x ^ ".") dep_replacement in
  let mess = with_default (fun x -> " " ^ x ^ ".") dep_message in
  dep ^ " was depreciated in version "  ^ dep_version ^ "." ^ repl ^ mess

open Simple_utils

open Location
open Ast_typed

module H = Compile.Helpers

module V = struct
  type t = virtual_location Var.t

  let compare = Simple_utils.Var.compare
end

module S = Set.Make(V)

let cons_of_syntax = function
  | H.CameLIGO -> Predefined.Tree_abstraction.Cameligo.constant_to_string
  | H.PascaLIGO -> Predefined.Tree_abstraction.Pascaligo.constant_to_string
  | H.ReasonLIGO -> Predefined.Tree_abstraction.Reasonligo.constant_to_string

let adds xs acc = List.fold_left (fun acc x -> S.add x acc) acc xs

let rec expression dep syntax defs x =
  let expression' defs x = expression dep syntax defs x in
  let add_if_eq e =
    if Var.equal dep e then [x.location] else []  in
  match x.expression_content with
  | E_literal _ | E_constructor _ ->
     []
  | E_constant {cons_name; arguments} ->
     let xs = List.(concat (map (expression' defs) arguments)) in
     let x = add_if_eq (Var.of_name (cons_of_syntax syntax cons_name)) in
     x@xs
  | E_variable v ->
     add_if_eq (unwrap v)
  | E_application {lamb;args} ->
     expression' defs lamb @ expression' defs args
  | E_lambda {binder; result} ->
     expression' (S.add (unwrap binder) defs) result
  | E_recursive {fun_name;lambda;_} ->
     expression' (adds [unwrap fun_name; unwrap lambda.binder] defs) lambda.result
  | E_let_in {let_binder;rhs;let_result;_} ->
     let rhs = expression' defs rhs in
     let let_result = expression' (S.add (unwrap let_binder) defs) let_result in
     rhs @ let_result
  | E_raw_code {language;code} ->
     begin match Compile.Helpers.syntax_to_variant (H.Syntax_name language) None with
     | Error _e -> failwith "TODO"
     | Ok (syntax',_) -> expression dep syntax' defs code end
  | E_matching {matchee;cases} ->
     let xs = expression' defs matchee in
     xs @ matching_cases expression' defs cases
  | E_record re ->
     Stage_common.Types.LMap.fold (fun _ x acc -> acc @ expression' defs x) re []
  | E_record_accessor {record;_} ->
     expression' defs record
  | E_record_update {record;update;_} ->
     expression' defs record @  expression' defs update

and matching_cases expression' defs = function
  | Match_list {match_nil;match_cons} ->
     let {hd;tl;body;_} = match_cons in
     expression' defs match_nil @ expression' (adds [unwrap hd; unwrap tl] defs) body
  | Match_option {match_none;match_some} ->
     let {opt;body;_} = match_some in
      expression' defs match_none @ expression' (S.add (unwrap opt) defs) body
  | Match_variant {cases;_} ->
     List.(concat (map (fun {pattern;body;_} -> expression' (S.add (unwrap pattern) defs) body) cases))

let get_depreciated (dep : V.t) syntax program =
  let aux ((defs,xs) as acc) (x : declaration_loc) =
    match unwrap x with
    | Declaration_constant {binder;expr;_} ->
       let defs' = S.add (unwrap binder) defs in
       let xs = xs@expression dep syntax defs expr in
       defs',xs
    | _ -> acc
  in
  snd (List.fold_left aux (S.empty,[]) program)

let run dep syntax program =
  let xs = get_depreciated (Var.of_name dep.Rules.dep) syntax program in
  List.map (fun x -> x,make_dep_msg dep) xs
