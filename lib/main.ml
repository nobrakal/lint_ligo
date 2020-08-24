open Rules
open Utils

open Compile.Linter
open Simple_utils.Trace

(* Parse and run a pattern matching *)
let pattern ?(debug=false) {pat; pat_type; pat_message} ast =
  let unparsed_pattern =
    Parser.unparsed_pattern Lexer_unparsed.token (Lexing.from_string pat) in
  match Unparser.Unparser_cameligo.node_of_string pat_type with
  | None -> Error (Errors.Bad_type pat_type)
  | Some typ ->
     if debug then
       print_endline ("PAT: " ^ Pattern.string_of_pattern unparsed_pattern);
     if debug then
       print_endline ("AST: " ^ Unparser.Ast.string_of_ast Unparser.Unparser_cameligo.string_of_node ast);
     let pat_result = Pattern.pat_match ~debug unparsed_pattern typ ast in
     Ok (Option.map (fun x -> x,pat_message) pat_result)

let run_pattern unparsed pat =
  Result.map filter_some (sequence_result (List.map (pattern pat) unparsed))

(* Run the rule on the given AST *)
let run_cst ast x =
  let unparsed = Unparser.Unparser_cameligo.unparse_cst ast in
  match x with
  | Depreciate _ ->
     Ok []
  | Pattern pat ->
     run_pattern unparsed pat

let run_typed ast x =
  match x with
  | Depreciate dep ->
     Ok (Depreciate.run dep Compile.Helpers.CameLIGO ast)
  | Pattern _ ->
     Ok []

let main run rules ast =
  Result.map List.concat @@ sequence_result @@ List.map (run ast) rules

let parse_rules buf =
  Parser.rules Lexer.token buf

let bind_compiler_result x f = match x with
  | Error e -> Error (Errors.Compiler e)
  | Ok x -> f (fst x)

let run rules = function
  | Cst (Camel_cst ast) -> main run_cst rules ast
  | Typed ast -> main run_typed rules ast
  | _ -> Ok []

let serialize result =
  let yojson_result = linter_result_to_yojson result in
  Yojson.Safe.to_string yojson_result

let main_serialized ~rules ~ast =
  match ast_of_yojson (Yojson.Safe.from_string ast) with
  | Error e ->
     Error (Errors.Ast_parsing e)
  | Ok ast ->
     Result.map
       (function
        | [] -> None
        | result -> Some (serialize result))
     @@ run (parse_rules rules) ast

let parse_file file =
  let%bind syntax = Compile.Helpers.(syntax_to_variant (Syntax_name "auto") (Some file)) in
  Compile.Helpers.(parse_and_abstract syntax file)

let string_of_result (loc,x) =
  let buff = Buffer.create 42 in
  let format = Format.formatter_of_buffer buff in
  Simple_utils.Location.pp format loc;
  Format.pp_print_flush format ();
  Buffer.add_string buff (":\n" ^ x);
  Buffer.contents buff

let main_file ~rules ~file =
  bind_compiler_result (parse_file file)
  @@ fun (_,cst) ->
     Result.map
       (function
        | [] -> None
        | results ->
           let results = String.concat "\n" (List.map string_of_result results) in
           Some results)
     @@ run (parse_rules rules) (Cst cst)
