open Rules

let run _ast = function
  | Depreciate _depreciate -> failwith "todo"
  | Pattern {pat;pat_message} ->
     ignore pat_message;
     let _unparsed_pattern =
       Parser.unparsed_pattern Lexer_unparsed.token (Lexing.from_string pat) in
     failwith "todo"

let main rules ast =
  List.map (run ast) rules

let parse_rules buf =
  Parser.rules Lexer.token buf

let parse_ast str_ast =
  Mini_c.program_of_yojson (Yojson.Safe.from_string str_ast)

let serialize result =
  let yojson_result = `List (List.map Simple_utils.Trace.annotation_to_yojson result) in
  Yojson.Safe.to_string yojson_result

let main_serialized ~rules ~ast =
  let result = main (parse_rules rules) (parse_ast ast) in
  serialize result
