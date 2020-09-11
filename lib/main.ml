open Rules
open Utils

type cst =
  | Camel_cst of Cameligo.CST.t
  | Pascal_cst of Pascaligo.CST.t
  | Reason_cst of Reasonligo.CST.t

module Pat_cameligo   = Run_pattern.Make(Unparser.Unparser_cameligo)
module Pat_pascaligo  = Run_pattern.Make(Unparser.Unparser_pascaligo)
module Pat_reasonligo = Run_pattern.Make(Unparser.Unparser_reasonligo)

let parse_rules buf =
  try Rules.rules_of_parsed @@ Lint_parser.rules Lexer.token buf
  with Lint_parser.Error -> Error Errors.RulesParsing

let run_imperative program =
  Ok (Deprecate.(format @@ run program))

let run_typed ?(entrypoint="_") deps program =
  let typed_result = Deprecate_custom.(format deps @@ run deps program) in
  let unused       = Unused_variable.(format @@ run ~program ~entrypoint) in
  Ok (typed_result @ unused)

let run_cst lang pats cst =
  let main run cst =
    Result.map List.concat @@ sequence_result @@ List.map (run cst) pats in
  match cst,lang with
  | Camel_cst  cst, Compile.Helpers.CameLIGO   ->
     main Pat_cameligo.run_cst  cst
  | Pascal_cst cst, Compile.Helpers.PascaLIGO  ->
     main Pat_pascaligo.run_cst cst
  | Reason_cst cst, Compile.Helpers.ReasonLIGO ->
     main Pat_reasonligo.run_cst cst
  | _ ->
     Error Errors.TypeMismatch

let from_compiler_result x = match x with
  | Ok (x,_) -> Ok x
  | Error e  -> Error (Errors.Compiler e)

let cst_of_file syntax file =
  let open Compile.Helpers in
  match syntax with
  | CameLIGO ->
     let%bind cst =
       from_compiler_result @@
         Simple_utils.Trace.trace Main_errors.parser_tracer @@
           Parser.Cameligo.parse_file file in
     Ok (Camel_cst cst)
  | PascaLIGO ->
     let%bind cst =
       from_compiler_result @@
         Simple_utils.Trace.trace Main_errors.parser_tracer @@
           Parser.Pascaligo.parse_file file in
     Ok (Pascal_cst cst)
  | ReasonLIGO ->
     let%bind cst =
       from_compiler_result @@
         Simple_utils.Trace.trace Main_errors.parser_tracer @@
           Parser.Reasonligo.parse_file file in
     Ok (Reason_cst cst)

let imperative_of_cst cst =
  from_compiler_result @@
    match cst with
    | Camel_cst raw ->
       Simple_utils.Trace.trace Main_errors.cit_cameligo_tracer @@
         Tree_abstraction.Cameligo.compile_program raw
    | Pascal_cst raw ->
       Simple_utils.Trace.trace Main_errors.cit_pascaligo_tracer @@
         Tree_abstraction.Pascaligo.compile_program raw
    | Reason_cst raw ->
       Simple_utils.Trace.trace Main_errors.cit_reasonligo_tracer @@
         Tree_abstraction.Reasonligo.compile_program raw

let parse_file syntax file =
  let open Compile.Helpers in
  let%bind cst = cst_of_file syntax file in
  let%bind imperative = imperative_of_cst cst in
  let%bind imperative =
    from_compiler_result @@
      Simple_utils.Trace.trace Main_errors.self_ast_imperative_tracer @@
        Self_ast_imperative.all_program imperative in
  Ok (imperative,cst)

let compile_to_typed entry_point imperative =
  let%bind sugar   = from_compiler_result @@ Compile.Of_imperative.compile imperative in
  let%bind core    = from_compiler_result @@ Compile.Of_sugar.compile sugar in
  let%bind typed,_ = from_compiler_result @@ Compile.Of_core.(compile (Contract entry_point) core) in
  Ok typed

let string_of_result (loc,x) =
  let buff = Buffer.create 42 in
  let format = Format.formatter_of_buffer buff in
  Simple_utils.Location.pp format loc;
  Format.pp_print_flush format ();
  Buffer.add_string buff (":\n" ^ x);
  Buffer.contents buff

let prepare_result =
  list_map_to_opt @@
    fun result ->
    let result = List.sort (fun (x,_) (y,_) -> Simple_utils.Location.compare x y) result in
    String.concat "\n" (List.map string_of_result result)

let main ~rules ~file ~entrypoint =
  let%bind syntax =
    from_compiler_result @@ Compile.Helpers.(syntax_to_variant (Syntax_name "auto") (Some file)) in
  let%bind {lang;pats;deps} = parse_rules rules in
  let%bind (imperative,cst) = parse_file syntax file in
  let%bind typed = compile_to_typed entrypoint imperative in
  let%bind result_imp   = run_imperative imperative in
  let%bind result_cst   = run_cst lang pats cst in
  let%bind result_typed = run_typed ~entrypoint deps typed in
  Ok (prepare_result (result_imp @ result_cst @ result_typed))
