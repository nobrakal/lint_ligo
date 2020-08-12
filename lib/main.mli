val main : Rules.rule list -> Mini_c.program -> Simple_utils.Trace.annotation list

val main_serialized : rules:Lexing.lexbuf -> ast:string -> string
