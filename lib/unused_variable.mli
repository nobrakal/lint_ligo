open Simple_utils

(* Return the unused variables of the given program. *)
val run :
  program:Ast_typed.program -> entrypoint:string -> Ast_typed.expression_variable list

val format :
  Ast_typed.expression_variable list -> (Location.t * string) list
