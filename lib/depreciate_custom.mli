(* Search for a custom depreciated function name and return all its uses. *)
val run :
  Rules.depreciate -> Ast_typed.program -> Simple_utils.Location.t list

val format :
   Rules.depreciate -> Simple_utils.Location.t list -> (Simple_utils.Location.t * string) list
