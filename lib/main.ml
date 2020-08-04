let main _ast = []

let main_serialized str_ast =
  let ast = Ast_imperative.program_of_yojson (Yojson.Safe.from_string str_ast) in
  let result = main ast in
  let yojson_result = `List (List.map Simple_utils.Trace.annotation_to_yojson result) in
  Yojson.Safe.to_string yojson_result
