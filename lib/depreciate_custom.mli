(* Search for a custom depreciated function name and return all its uses. *)
val run :
  Rules.depreciate list -> Ast_typed.program -> (string * Simple_utils.Location.t) list

val format :
   Rules.depreciate list -> (string * Simple_utils.Location.t) list -> (Simple_utils.Location.t * string) list
