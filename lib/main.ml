open Rules
open Utils

open Compile.Linter

module Pat_cameligo  = Run_pattern.Make(Unparser.Unparser_cameligo)
module Pat_pascaligo = Run_pattern.Make(Unparser.Unparser_pascaligo)

let run_typed lang ast dep =
  Ok (Depreciate.run dep lang ast)

let main run rules ast =
  Result.map List.concat @@ sequence_result @@ List.map (run ast) rules

let parse_rules buf =
  Rules.rules_of_parsed @@ Parser.rules Lexer.token buf

let run {lang;deps;pats} = function
  | Typed ast -> main (run_typed lang) deps ast
  | Cst cst ->
     match cst,lang with
     | Camel_cst  cst, Compile.Helpers.CameLIGO   ->
        main Pat_cameligo.run_cst  pats cst
     | Pascal_cst cst, Compile.Helpers.PascaLIGO  ->
        main Pat_pascaligo.run_cst pats cst
     | Reason_cst _  , Compile.Helpers.ReasonLIGO ->
        failwith "ReasonLIGO"
     | _ ->
        Error Errors.TypeMismatch

let serialize result =
  Yojson.Safe.to_string @@ linter_result_to_yojson result

let main_serialized ~rules ~ast =
  match ast_of_yojson (Yojson.Safe.from_string ast) with
  | Error e ->
     Error (Errors.Ast_parsing e)
  | Ok ast ->
     let%bind rules = parse_rules rules in
     let%bind result = run rules ast in
     Ok (list_map_to_opt serialize result)

let from_compiler_result x = match x with
  | Ok (x,_) -> Ok x
  | Error e  -> Error (Errors.Compiler e)

let parse_file file =
  let%bind syntax =
    from_compiler_result @@ Compile.Helpers.(syntax_to_variant (Syntax_name "auto") (Some file)) in
  from_compiler_result @@ Compile.Helpers.(parse_and_abstract syntax file)

let compile_to_typed entry_point imperative =
  let%bind sugar   = from_compiler_result @@ Compile.Of_imperative.compile imperative in
  let%bind core    = from_compiler_result @@ Compile.Of_sugar.compile sugar in
  let%bind  typed,_ = from_compiler_result @@ Compile.Of_core.(compile (Contract entry_point) core) in
  Ok typed

let string_of_result (loc,x) =
  let buff = Buffer.create 42 in
  let format = Format.formatter_of_buffer buff in
  Simple_utils.Location.pp format loc;
  Format.pp_print_flush format ();
  Buffer.add_string buff (":\n" ^ x);
  Buffer.contents buff

let prepare_result_file =
  list_map_to_opt @@
    fun result ->  String.concat "\n" (List.map string_of_result result)

let main_file ~rules ~file ~entry_point =
  let%bind rules   = parse_rules rules in
  let%bind (imperative,cst) = parse_file file in
  let%bind ast = compile_to_typed entry_point imperative in
  let%bind result_cst = run rules (Cst cst) in
  let%bind result_ast = run rules (Typed ast) in
  Ok (prepare_result_file (result_cst @ result_ast))
