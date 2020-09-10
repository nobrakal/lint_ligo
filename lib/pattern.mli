(* The type of patterns.*)
type 'a pattern =
  | Pat_lex of string
  | Pat_pat of 'a pattern list
  | Pat_var of string * 'a option

val string_of_pattern : 'a pattern -> string
val map_type_pattern : ('a -> 'b) -> 'a pattern -> 'b pattern

(* Givan a pattern and a type, pattern-match the given AST. *)
val pat_match : ?debug:bool -> 'a pattern -> 'a -> 'a Unparser.Ast.t -> Simple_utils.Location.t option
