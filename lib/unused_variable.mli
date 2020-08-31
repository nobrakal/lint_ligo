open Simple_utils

val unused_variables_of_program :
  program:Ast_typed.program -> entrypoint:string -> string Var.t Location.wrap list

val make_warnings :
  string Var.t Location.wrap list -> (Location.t * string) list
