open Rules
open Compile.Linter

let filter_some xs =
  List.fold_right (fun x acc -> match x with None -> acc | Some x -> x::acc) xs []

let make_dep_msg {dep; dep_version; dep_replacement; dep_message} =
  let with_default f = Option.fold ~none:"" ~some:f in
  let repl = with_default (fun x -> "A possible replacement is " ^ x ^ ".") dep_replacement in
  let mess = with_default (fun x -> " " ^ x ^ ".") dep_message in
  dep ^ " was depreciated in version "  ^ dep_version ^ "." ^ repl ^ mess

let pattern {pat; pat_type; pat_message} ast =
  let unparsed_pattern =
    Parser.unparsed_pattern Lexer_unparsed.token (Lexing.from_string pat) in
  let typ = Unparser_cameligo.node_of_string' pat_type in
  Option.map (fun x -> x,pat_message) (Pattern.pat_match unparsed_pattern typ ast)

let run ast x =
  let unparsed = Unparser_cameligo.unparse_cst ast in
  match x with
  | Depreciate dep ->
     let pat = Pattern.Pat_lex dep.dep in
     List.map (fun x -> x,make_dep_msg dep)
       (filter_some (List.map (Pattern.pat_match pat Unparser_cameligo.Name) unparsed))
  | Pattern pat ->
     filter_some (List.map (pattern pat) unparsed)

let main rules ast =
  List.(concat (map (run ast) rules))

let parse_rules buf =
  Parser.rules Lexer.token buf

let parse_and_run rules str_ast =
  let run = function
    | Cst (Camel_cst ast) -> main rules ast
    | _ -> [] in
  Result.map run (ast_of_yojson (Yojson.Safe.from_string str_ast))

let serialize result =
  let yojson_result = linter_result_to_yojson result in
  Yojson.Safe.to_string yojson_result

let main_serialized ~rules ~ast =
  match parse_and_run (parse_rules rules) ast with
  | Error e -> failwith e
  | Ok result ->
     match result with
     | [] -> None
     | result -> Some (serialize result)
