val main : Rules.rule list -> Ast_typed.program -> Simple_utils.Trace.linter_annotation list

val main_serialized : rules:Lexing.lexbuf -> ast:string -> string option
