open Utils
open Simple_utils

open Ast_typed

(* We go through the Typed AST and maintain a map
   from variables to a boolean indicating if the variable
   was used.
   To deal with name capture, a list of known unused variables is
   also maintained.
*)

module V = struct

  type t = expression_variable

  (* We compare variables _modulo_ their location. *)
  let compare x y = Var.compare (Location.unwrap x) (Location.unwrap y)
end

module S = Set.Make(V)
module M = Map.Make(V)

(* A map recording if a variable is being used * a list of unused variables. *)
type defuse = bool M.t * V.t list

(* This function also returns the original key, as it contains the original location. *)
let find_opt target m =
  let aux k v x =
    match x with
    | None ->
       if V.compare target k = 0
       then Some (k,v) else None
    | Some _ -> x
  in
  M.fold aux m None

let defuse_union (x,a) (y,b) =
  M.union (fun _ x y -> Some (x||y)) x y, a@b

let defuse_neutral =
  (M.empty,[])

let defuse_unions defuse =
  List.fold_left defuse_union (defuse,[])

let replace_opt k x m =
  Stdlib.Option.fold ~none:(M.remove k m) ~some:(fun x -> M.add k x m) x

let add_if_not_generated ?forbidden x xs b =
  let v = Location.unwrap x in
  let sv = string_of_var v in
  if not b && not (Var.is_generated v)
     && (String.get sv 0) <> '_'
     && Stdlib.Option.fold ~none:true ~some:(fun x -> x <> sv) forbidden
  then x::xs else xs

let remove_defined_var_after defuse binder f expr =
  let old_binder = M.find_opt binder defuse in
  let defuse,unused = f (M.add binder false defuse) expr in
  let unused = add_if_not_generated binder unused (M.find binder defuse) in
  replace_opt binder old_binder defuse, unused

let add_if_unused unused binder defuse =
  match find_opt binder defuse with
  | None -> unused
  | Some (k,b) ->
     add_if_not_generated k unused b

(* Return a def-use graph + a list of unused variables *)
let rec defuse_of_expr defuse expr : defuse =
  match expr.expression_content with
  | E_literal _ ->
     defuse,[]
  | E_constructor {element;_} ->
     defuse_of_expr defuse element
  | E_constant {arguments;_} ->
     defuse_unions defuse (List.map (defuse_of_expr defuse) arguments)
  | E_variable v ->
     M.add v true defuse,[]
  | E_application {lamb;args} ->
     defuse_union (defuse_of_expr defuse lamb) (defuse_of_expr defuse args)
  | E_lambda l ->
     defuse_of_lambda defuse l
  | E_recursive {fun_name;lambda;_} ->
     remove_defined_var_after defuse fun_name defuse_of_lambda lambda
  | E_let_in {let_binder;rhs;let_result;_} ->
     let defuse,unused = defuse_of_expr defuse rhs in
     let old_binder = M.find_opt let_binder defuse in
     let defuse, unused' = defuse_of_expr (M.add let_binder false defuse) let_result in
     let unused' = add_if_unused unused' let_binder defuse in
     replace_opt let_binder old_binder defuse, unused@unused'
  | E_raw_code {code;_} ->
     defuse_of_expr defuse code
  | E_matching {matchee;cases} ->
     defuse_union (defuse_of_expr defuse matchee) (defuse_of_cases defuse cases)
  | E_record re ->
     Stage_common.Types.LMap.fold
       (fun _ x -> defuse_union (defuse_of_expr defuse x)) re defuse_neutral
  | E_record_accessor {record;_} ->
     defuse_of_expr defuse record
  | E_record_update {record;update;_} ->
     defuse_union (defuse_of_expr defuse record) (defuse_of_expr defuse update)

and defuse_of_lambda defuse {binder; result} =
  remove_defined_var_after defuse binder defuse_of_expr result

and defuse_of_cases defuse = function
  | Match_list    x -> defuse_of_plist defuse x
  | Match_option  x -> defuse_of_poption defuse x
  | Match_variant x -> defuse_of_variant defuse x

and defuse_of_plist defuse {match_nil;match_cons} =
  let {hd;tl;body;_} = match_cons in
  let hd' = M.find_opt hd defuse in
  let tl' = M.find_opt tl defuse in
  let defuse,unused = defuse_of_expr (M.add hd false (M.add tl false defuse)) body in
  let unused = add_if_not_generated hd unused (M.find hd defuse) in
  let unused = add_if_not_generated tl unused (M.find tl defuse) in
  let defuse = replace_opt hd hd' (replace_opt tl tl' defuse) in
  defuse_union (defuse,unused) (defuse_of_expr defuse match_nil)

and defuse_of_poption defuse {match_none;match_some} =
  let {opt;body;_} = match_some in
  defuse_union (defuse_of_expr defuse match_none)
    (remove_defined_var_after defuse opt defuse_of_expr body)

and defuse_of_variant defuse {cases;_} =
  defuse_unions defuse @@
    List.map
      (fun {pattern;body;_} ->
        remove_defined_var_after defuse pattern defuse_of_expr body)
      cases

let defuse_of_program : program -> defuse = fun xs ->
  let aux ((defuse, unused') as acc) x =
    match Location.unwrap x with
    | Declaration_constant {binder;expr;_} ->
       let defuse,unused = defuse_of_expr defuse expr in
       let unused = add_if_unused unused binder defuse in
       M.add binder false defuse, unused @ unused'
    | _ -> acc (* TODO unused types ? *)
  in List.fold_left aux defuse_neutral xs

let run ~program ~entrypoint : expression_variable list =
  let defuse,unused = defuse_of_program program in
  M.fold (fun v b acc -> add_if_not_generated ~forbidden:entrypoint v acc b) defuse unused

let format xs =
  let xs =
    List.sort (fun x y -> Location.(compare (get_location x) (get_location y))) xs in
  let aux v =
    Location.get_location v, "Unused variable " ^ string_of_var (Location.unwrap v) ^ "." in
  List.map aux xs
