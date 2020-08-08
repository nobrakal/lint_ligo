let main _rules _ast = []

let parse_rules buf =
  Parser.rules Lexer.token buf

let parse_ast str_ast =
  Ast_imperative.program_of_yojson (Yojson.Safe.from_string str_ast)

let serialize result =
  let yojson_result = `List (List.map Simple_utils.Trace.annotation_to_yojson result) in
  Yojson.Safe.to_string yojson_result

let main_serialized ~rules ~ast =
  let result = main (parse_rules rules) (parse_ast ast) in
  serialize result
