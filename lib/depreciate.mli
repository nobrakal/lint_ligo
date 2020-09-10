(** Returns all the constants marked as deprecated used in the AST. *)
val program : Ast_imperative.program -> (Simple_utils.Location.t * string) list
