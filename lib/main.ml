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

let bind_compiler_result x f = match x with
  | Error e -> Error (Errors.Compiler e)
  | Ok x -> f (fst x)

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

let parse_file file =
  match Compile.Helpers.(syntax_to_variant (Syntax_name "auto") (Some file)) with
  | Ok (syntax,_) -> Compile.Helpers.(parse_and_abstract syntax file)
  | Error e -> Error e

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

let main_file ~rules ~file =
  bind_compiler_result (parse_file file)
  @@ fun (_,cst) ->
     let%bind rules = parse_rules rules in
     let%bind result = run rules (Cst cst) in
     Ok (prepare_result_file result)
