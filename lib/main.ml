open Rules
open Compile.Linter

let filter_some xs =
  List.fold_right (fun x acc -> match x with None -> acc | Some x -> x::acc) xs []

let pattern ast {pat; pat_type; pat_message} =
  let unparsed_pattern =
    Parser.unparsed_pattern Lexer_unparsed.token (Lexing.from_string pat) in
  let typ = Unparser_cameligo.node_of_string' pat_type in
  if Pattern.pat_match unparsed_pattern typ ast
  then Some (Simple_utils.Location.generated, pat_message)
  else None

let run ast = function
  | Depreciate dep -> Depreciate.depreciate ast dep
  | Pattern pat ->
     let unparsed = Unparser_cameligo.unparse_cst ast in
     pattern unparsed pat

let main rules ast =
  filter_some @@ List.map (run ast) rules

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
