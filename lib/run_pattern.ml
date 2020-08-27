open Unparser
open Rules

open Utils

module Make(U:Unparse.Unparser)= struct

  exception Bad_node of string

  let string_of_node' x =
    match U.node_of_string x with
    | None -> raise (Bad_node x)
    | Some x -> x

  (* Parse and run a pattern matching *)
  let pattern ?(debug=true) {pat; pat_type; pat_message} ast =
    let unparsed_pattern =
      Parser.unparsed_pattern Lexer_unparsed.token (Lexing.from_string pat) in
    try
      let unparsed_pattern =
        Pattern.map_type_pattern string_of_node' unparsed_pattern in
      let typ = string_of_node' pat_type in
      let pat_result = Pattern.pat_match ~debug unparsed_pattern typ ast in
      Ok (Option.map (fun x -> x,pat_message) pat_result)
    with
    | Bad_node x -> Error (Errors.Bad_type x)

  let run_pattern unparsed pat =
    Result.map filter_some (sequence_result (List.map (pattern pat) unparsed))

  (* Run the rule on the given AST *)
  let run_cst ast pat =
    run_pattern (U.unparse_cst ast) pat
end
