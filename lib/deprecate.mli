(* Returns all the constants marked as deprecated used in the AST. *)
val run : Ast_imperative.program -> (Simple_utils.Location.t * string) list

(* Format the result of @run@ *)
val format : (Simple_utils.Location.t * string) list -> (Simple_utils.Location.t * string) list
