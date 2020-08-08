val main : Rules.rule list -> Ast_imperative.program -> Simple_utils.Trace.annotation list

val main_serialized : rules:Lexing.lexbuf -> ast:string -> string
