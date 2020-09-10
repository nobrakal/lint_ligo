(* Search for a custom depreciated function name and return all its uses. *)
val run :
  Rules.depreciate -> Ast_typed.program -> (Simple_utils.Location.t * string) list
