open Simple_utils

val unused_variables_of_program :
  program:Ast_typed.program -> entrypoint:string -> Ast_typed.expression_variable list

val make_warnings :
  Ast_typed.expression_variable list -> (Location.t * string) list
