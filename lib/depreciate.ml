open Rules

open Mini_c

module SMap = Map.Make(String)

let is_equal_to_wrapped_var x y =
  Simple_utils.Var.(equal (Simple_utils.Location.unwrap x) (of_name y))

let search_depreciated (dep : string) (ast: Mini_c.program) =
  let rec expression {content;location;_} =
    match content with
    | E_literal _ | E_constant _ -> []
    | E_closure {binder;body} ->
       if is_equal_to_wrapped_var binder dep then [] else expression body
    | E_application (l,r) -> expression l @ expression r
    | E_variable name ->
       if is_equal_to_wrapped_var name dep then [location] else []
    | E_if_bool (i,f,e) -> expression i @ expression f @ expression e
    | E_raw_michelson name ->
       if dep = name then [location] else []
    | E_let_in ((name,_),_,expr1,expr2) ->
       expression expr2 @
         if is_equal_to_wrapped_var name dep then expression expr1 else []
    | _ -> failwith "todo" in
  let aux_program xs ((var_name,_,expr),_) =
    if is_equal_to_wrapped_var var_name dep
    then [] (* Remove subsequent depreciated *)
    else expression expr@xs in
  List.rev (List.fold_left aux_program [] ast)

let make_msg {dep; dep_version; dep_replacement; dep_message} =
  let with_default f = Option.fold ~none:"" ~some:f in
  let repl = with_default (fun x -> "A possible replacement is " ^ x ^ ".") dep_replacement in
  let mess = with_default (fun x -> " " ^ x ^ ".") dep_message in
  dep ^ " was depreciated in version "  ^ dep_version ^ "." ^ repl ^ mess

let depreciate ast depreciated =
  let msg = make_msg depreciated in
  let lst = search_depreciated depreciated.dep ast in
  List.map (fun x -> (x,msg)) lst
