open Rules

open Compile.Linter
open Simple_utils.Trace

let filter_some xs =
  List.fold_right (fun x acc -> match x with None -> acc | Some x -> x::acc) xs []

(* Create the message for a depreciated function *)
let make_dep_msg {dep; dep_version; dep_replacement; dep_message} =
  let with_default f = Option.fold ~none:"" ~some:f in
  let repl = with_default (fun x -> "A possible replacement is " ^ x ^ ".") dep_replacement in
  let mess = with_default (fun x -> " " ^ x ^ ".") dep_message in
  dep ^ " was depreciated in version "  ^ dep_version ^ "." ^ repl ^ mess

(* Parse and run a pattern matching *)
let pattern ?(debug=false) {pat; pat_type; pat_message} ast =
  let unparsed_pattern =
    Parser.unparsed_pattern Lexer_unparsed.token (Lexing.from_string pat) in
  let typ = Unparser_cameligo.node_of_string' pat_type in
  if debug then print_endline ("PAT: " ^ Pattern.string_of_pattern unparsed_pattern);
  if debug then print_endline ("AST: " ^ Pattern.string_of_ast Unparser_cameligo.string_of_node ast);
  Option.map (fun x -> x,pat_message) (Pattern.pat_match ~debug unparsed_pattern typ ast)

let run_depreciate unparsed dep =
  let pat = Pattern.Pat_lex dep.dep in
  List.map (fun x -> x,make_dep_msg dep)
    (filter_some (List.map (Pattern.pat_match pat Unparser_cameligo.Name) unparsed))

let run_pattern unparsed pat =
  filter_some (List.map (pattern pat) unparsed)

(* Run the rule on the given AST *)
let run ast x =
  let unparsed = Unparser_cameligo.unparse_cst ast in
  match x with
  | Depreciate dep ->
     run_depreciate unparsed dep
  | Pattern pat ->
     run_pattern unparsed pat

let main rules ast =
  List.(concat (map (run ast) rules))

let parse_rules buf =
  Parser.rules Lexer.token buf

let run rules = function
  | Cst (Camel_cst ast) -> main rules ast
  | _ -> []

let serialize result =
  let yojson_result = linter_result_to_yojson result in
  Yojson.Safe.to_string yojson_result

let main_serialized ~rules ~ast =
  match ast_of_yojson (Yojson.Safe.from_string ast) with
  | Error e -> failwith e
  | Ok ast ->
     match run (parse_rules rules) ast with
     | [] -> None
     | result -> Some (serialize result)

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
  match parse_file file with
  | Error _ -> assert false
  | Ok ((_,cst),_) ->
     match run (parse_rules rules) (Cst cst) with
     | [] -> None
     | results ->
        let results = String.concat "\n" (List.map string_of_result results) in
        Some results
