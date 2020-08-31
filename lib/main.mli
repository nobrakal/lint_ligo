val main_serialized : rules:Lexing.lexbuf -> ast:string -> (string option, Errors.t) result

val main_file : rules:Lexing.lexbuf -> file:string -> entrypoint:string -> (string option, Errors.t) result
