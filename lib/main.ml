open Rules

let pattern _ast {pat; pat_message} =
  ignore pat_message;
  let _unparsed_pattern =
    Parser.unparsed_pattern Lexer_unparsed.token (Lexing.from_string pat) in
  failwith "todo"

let run ast = function
  | Depreciate dep -> Depreciate.depreciate ast dep
  | Pattern pat -> pattern ast pat

let main rules ast =
  List.concat @@ List.map (run ast) rules

let parse_rules buf =
  Parser.rules Lexer.token buf

let parse_ast str_ast =
  Mini_c.program_of_yojson (Yojson.Safe.from_string str_ast)

let serialize result =
  let yojson_result = `List (List.map Simple_utils.Trace.linter_annotation_to_yojson result) in
  Yojson.Safe.to_string yojson_result

let main_serialized ~rules ~ast =
  match parse_ast ast with
  | Error e -> failwith e
  | Ok ast ->
     match main (parse_rules rules) ast with
     | [] -> None
     | result -> Some (serialize result)
